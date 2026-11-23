#!/usr/bin/env node
/**
 * agent-bus-wait: a transparent stdio MCP proxy around the ghostbus 0.5.x
 * stdio server.
 *
 * Why this exists: ghostbus is pull-only. Its stdio server implements
 * initialize / tools/list / tools/call and nothing else, so an MCP client cannot
 * be woken when a message arrives. The package's HTTP relay has /api/wait and
 * SSE, but that process is not running. This proxy keeps the existing server
 * (same binary, same --store) and adds one local tool:
 *
 *   bus_wait({ agent, timeoutMs?, sinceSeq?, afterId?, unreadOnly? })
 *
 * It watches the file-backed store, blocks until a direct or broadcast message
 * relevant to `agent` (never its own) appears, and returns the message(s)
 * WITHOUT marking them read. A timeout is a normal, non-error result.
 *
 * Everything else is forwarded byte-for-byte: JSON-RPC requests, responses and
 * notifications. The only response rewritten is tools/list, which gains the
 * bus_wait definition.
 *
 * Usage:
 *   node proxy.mjs --store /path/to/bus.json [-- child argv...]
 *
 * With no `--`, the child defaults to:
 *   npx -y ghostbus --store /path/to/bus.json
 *
 * Zero dependencies. Node >= 20.
 */
import fs from 'node:fs';
import fsp from 'node:fs/promises';
import path from 'node:path';
import readline from 'node:readline';
import { spawn } from 'node:child_process';

const TAG = '[agent-bus-wait]';
const DEFAULT_TIMEOUT_MS = 30_000;
const MAX_TIMEOUT_MS = 600_000;
// fs.watch delivers the atomic rename immediately; this is only the fallback
// interval for filesystems where inotify is unavailable or events are missed.
const FALLBACK_POLL_MS = 1_000;

const log = (...parts) => {
  try {
    process.stderr.write(`${TAG} ${parts.join(' ')}\n`);
  } catch {
    // stderr is gone; nothing useful left to do
  }
};

function safeWrite(stream, text) {
  try {
    stream.write(text);
  } catch (err) {
    if (err && err.code === 'EPIPE') process.exit(0);
    log(`write failed: ${err && err.message ? err.message : String(err)}`);
  }
}

const send = (obj) => safeWrite(process.stdout, `${JSON.stringify(obj)}\n`);
const sendResult = (id, result) => send({ jsonrpc: '2.0', id, result });
const sendError = (id, code, message) =>
  send({ jsonrpc: '2.0', id, error: { code, message } });
const toolResult = (value) => ({
  content: [{ type: 'text', text: JSON.stringify(value, null, 2) }],
});
const sendToolError = (id, message) =>
  sendResult(id, {
    content: [{ type: 'text', text: `Error: ${message}` }],
    isError: true,
  });

// JSON-RPC ids may be numbers, strings or null. Keep the type in the key so
// id 1 and id "1" cannot shadow each other.
const idKey = (id) => `${typeof id}:${String(id)}`;

function parseArgs(argv) {
  let store = process.env.GHOSTBUS_STORE || null;
  let childArgv = null;

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === '--') {
      childArgv = argv.slice(i + 1);
      break;
    }
    if (arg === '--store') {
      store = argv[i + 1];
      i += 1;
      continue;
    }
    if (arg.startsWith('--store=')) {
      store = arg.slice('--store='.length);
      continue;
    }
    if (arg === '--help' || arg === '-h') {
      process.stdout.write(
        'usage: node proxy.mjs --store FILE [-- child argv...]\n'
      );
      process.exit(0);
    }
    log(`ignoring unknown argument: ${arg}`);
  }

  const rawStore = store || './ghostbus-data.json';
  const storePath = rawStore === ':memory:' ? rawStore : path.resolve(rawStore);
  if (!childArgv || childArgv.length === 0) {
    childArgv = [
      process.env.GHOSTBUS_NPX || 'npx',
      '-y',
      'ghostbus',
      '--store',
      storePath,
    ];
  }
  return { storePath, childArgv };
}

const { storePath, childArgv } = parseArgs(process.argv.slice(2));

const BUS_WAIT_TOOL = {
  name: 'bus_wait',
  description:
    'Block until the next inbox-relevant message arrives, instead of polling. Waits for a direct message to this agent or a broadcast (to "*") from another agent, then returns the message(s) WITHOUT marking them read; call bus_inbox to read and mark them. By default it watches for messages that arrive after the call; pass sinceSeq (headSeq from a previous result) or afterId (a message id) to catch up. A timeout is a normal result with timedOut: true and an empty messages array.',
  annotations: {
    readOnlyHint: true,
    destructiveHint: false,
    idempotentHint: false,
    openWorldHint: false,
  },
  inputSchema: {
    type: 'object',
    required: ['agent'],
    properties: {
      agent: {
        type: 'string',
        description:
          'Your registered agent name. Direct messages to it and broadcasts are relevant; messages it sent itself are not.',
      },
      timeoutMs: {
        type: 'number',
        default: DEFAULT_TIMEOUT_MS,
        description:
          'How long to block before returning timedOut: true, in milliseconds. Default 30000; clamped to 600000. Keep it below the MCP client tool timeout (OpenCode agent-bus execution is 120000 ms).',
      },
      sinceSeq: {
        type: 'number',
        description:
          'Only consider messages whose send event sequence is greater than this. Use headSeq from a previous bus_wait or bus_sync result, or the highest event seq from bus_events.',
      },
      afterId: {
        type: 'number',
        description:
          'Only consider messages with an id greater than this. Use the id of the last message you saw. Applied together with sinceSeq when both are given.',
      },
      unreadOnly: {
        type: 'boolean',
        default: false,
        description:
          'When true, skip messages already listed in the agent\'s readBy. Default false (a newly arrived message is normally unread anyway).',
      },
    },
  },
};

function parseWaitArguments(raw) {
  const args = raw && typeof raw === 'object' && !Array.isArray(raw) ? raw : {};
  const agent = typeof args.agent === 'string' ? args.agent.trim() : '';
  if (!agent) throw new Error('`agent` is required (your registered bus name)');

  let timeoutMs = DEFAULT_TIMEOUT_MS;
  if (args.timeoutMs !== undefined && args.timeoutMs !== null) {
    const n = Number(args.timeoutMs);
    if (!Number.isFinite(n) || n < 0) {
      throw new Error('`timeoutMs` must be a non-negative number');
    }
    timeoutMs = Math.min(Math.floor(n), MAX_TIMEOUT_MS);
  }

  const cursor = (name) => {
    const value = args[name];
    if (value === undefined || value === null) return null;
    const n = Number(value);
    if (!Number.isFinite(n) || n < 0) {
      throw new Error(`\`${name}\` must be a non-negative number`);
    }
    return Math.floor(n);
  };

  return {
    agent,
    timeoutMs,
    sinceSeq: cursor('sinceSeq'),
    afterId: cursor('afterId'),
    unreadOnly: args.unreadOnly === true || args.unreadOnly === 'true',
  };
}

async function readBusState(file) {
  try {
    const text = await fsp.readFile(file, 'utf8');
    if (!text.trim()) return null;
    const state = JSON.parse(text);
    if (!state || typeof state !== 'object' || Array.isArray(state)) return null;
    return state;
  } catch {
    // ENOENT while the store is being created, or a torn/partial read on a
    // filesystem that is not doing atomic replacement. The next watch/poll
    // retries; never throw at the caller.
    return null;
  }
}

function maxMessageId(state) {
  let max = 0;
  if (Array.isArray(state.messages)) {
    for (const message of state.messages) {
      const id = Number(message && message.id);
      if (Number.isFinite(id) && id > max) max = id;
    }
  }
  const msgSeq = Number(state.msgSeq);
  if (Number.isFinite(msgSeq) && msgSeq > max) max = msgSeq;
  return max;
}

function sendEventSeqById(state) {
  const map = new Map();
  if (!Array.isArray(state.events)) return map;
  for (const event of state.events) {
    if (!event || event.type !== 'message.send') continue;
    const messageId = Number(event.messageId);
    const seq = Number(event.seq);
    if (!Number.isFinite(messageId) || !Number.isFinite(seq)) continue;
    const previous = map.get(messageId);
    if (previous === undefined || seq > previous) map.set(messageId, seq);
  }
  return map;
}

function computeBaseline(state, opts) {
  // afterId is the explicit message cursor. sinceSeq alone is an event cursor:
  // start at 0 and let the send-event filter select messages after it. With
  // neither, "new" means "arrived after this call", so snapshot both the
  // message counter and the highest present id.
  if (opts.afterId !== null) return opts.afterId;
  if (opts.sinceSeq !== null) return 0;
  return maxMessageId(state);
}

function collectMessages(state, opts, baseline, eventSeqById) {
  if (!Array.isArray(state.messages)) return [];
  const out = [];
  for (const message of state.messages) {
    if (!message || typeof message !== 'object') continue;
    const id = Number(message.id);
    if (!Number.isFinite(id) || id <= baseline) continue;
    if (message.from === opts.agent) continue; // never wake on our own sends
    if (message.to !== opts.agent && message.to !== '*') continue;
    if (
      opts.unreadOnly &&
      Array.isArray(message.readBy) &&
      message.readBy.includes(opts.agent)
    ) {
      continue;
    }
    if (opts.sinceSeq !== null) {
      const eventSeq = eventSeqById.get(id);
      // An absent send event means the message predates the retained event
      // log; it cannot be a message after sinceSeq.
      if (eventSeq === undefined || eventSeq <= opts.sinceSeq) continue;
    }
    out.push(message);
  }
  return out.sort((a, b) => Number(a.id) - Number(b.id));
}

/**
 * One watcher per bus_wait call. fs.watch is attached to the directory, not the
 * file, because FileStore.save() replaces the file by rename: a watch on the
 * old inode would go stale. The wait() timer is the fallback poll; it never
 * busy-loops (one wake per second at worst).
 */
class StoreWatcher {
  constructor(file) {
    this.dir = path.dirname(file);
    this.base = path.basename(file);
    this.waiters = new Set();
    this.dirty = false;
    this.closed = false;
    this.watcher = null;
    try {
      this.watcher = fs.watch(this.dir, { persistent: true }, (_event, filename) => {
        if (this.closed) return;
        const name = filename === null || filename === undefined ? null : String(filename);
        // Ignore unrelated files in the directory. Temp files are included so
        // a write in progress wakes the waiter once, not on every chunk.
        if (name && name !== this.base && !name.startsWith(`${this.base}.tmp.`)) {
          return;
        }
        this.notify();
      });
      this.watcher.on('error', (err) => {
        log(`store watch error: ${err.message}; falling back to polling`);
        try {
          this.watcher.close();
        } catch {
          // already closed
        }
        this.watcher = null;
      });
    } catch (err) {
      log(`cannot watch ${this.dir}: ${err.message}; falling back to polling`);
      this.watcher = null;
    }
  }

  notify() {
    if (this.closed) return;
    if (this.waiters.size === 0) {
      this.dirty = true;
      return;
    }
    for (const wake of [...this.waiters]) wake();
  }

  wait(ms, signal) {
    if (this.closed) return Promise.resolve();
    if (signal.aborted) return Promise.reject(signal.reason);
    if (this.dirty) {
      this.dirty = false;
      return Promise.resolve();
    }
    return new Promise((resolve, reject) => {
      let settled = false;
      let timer = null;
      const cleanup = () => {
        if (settled) return false;
        settled = true;
        if (timer !== null) clearTimeout(timer);
        this.waiters.delete(wake);
        signal.removeEventListener('abort', onAbort);
        return true;
      };
      const finish = (fn, value) => {
        if (cleanup()) fn(value);
      };
      const wake = () => finish(resolve);
      const onAbort = () => finish(reject, signal.reason ?? new Error('aborted'));
      timer = setTimeout(() => finish(resolve), Math.max(0, ms));
      this.waiters.add(wake);
      signal.addEventListener('abort', onAbort, { once: true });
      // The abort can race the addEventListener call.
      if (signal.aborted) onAbort();
    });
  }

  close() {
    this.closed = true;
    if (this.watcher) {
      try {
        this.watcher.close();
      } catch {
        // already closed
      }
      this.watcher = null;
    }
    for (const wake of [...this.waiters]) wake();
    this.waiters.clear();
  }
}

async function waitForBusMessage(opts, signal) {
  if (storePath === ':memory:') {
    throw new Error(
      'bus_wait requires a file-backed ghostbus store; this server uses :memory:'
    );
  }
  const start = Date.now();
  const deadline = start + opts.timeoutMs;
  const watcher = new StoreWatcher(storePath);
  let baseline = null;
  let headSeq = null;

  try {
    for (;;) {
      if (signal.aborted) throw signal.reason ?? new Error('aborted');
      const state = await readBusState(storePath);
      if (state) {
        const seq = Number(state.seq);
        headSeq = Number.isFinite(seq) ? seq : 0;
        if (baseline === null) baseline = computeBaseline(state, opts);
        const eventSeqById =
          opts.sinceSeq === null ? null : sendEventSeqById(state);
        const messages = collectMessages(state, opts, baseline, eventSeqById);
        if (messages.length > 0) {
          const lastId = Number(messages[messages.length - 1].id);
          return {
            timedOut: false,
            messages,
            headSeq,
            afterId: Number.isFinite(lastId) ? lastId : baseline,
          };
        }
      }

      const remaining = deadline - Date.now();
      if (remaining <= 0) {
        return {
          timedOut: true,
          messages: [],
          headSeq,
          afterId: baseline === null ? 0 : baseline,
        };
      }
      await watcher.wait(Math.min(remaining, FALLBACK_POLL_MS), signal);
    }
  } finally {
    watcher.close();
  }
}

// ---------------------------------------------------------------------------
// Child process: the unmodified ghostbus stdio MCP server
// ---------------------------------------------------------------------------
let child = null;
let childExited = false;
const pending = new Map(); // idKey -> { id, method }
const activeWaits = new Map(); // idKey -> AbortController

function failPending(message) {
  for (const entry of pending.values()) {
    sendError(entry.id, -32603, message);
  }
  pending.clear();
}

function abortWaits(reason) {
  for (const controller of activeWaits.values()) {
    controller.abort(reason);
  }
}

function handleChildGone(detail, exitCode = 0) {
  if (childExited) return;
  childExited = true;
  log(`ghostbus child exited (${detail})`);
  failPending(`ghostbus server exited (${detail})`);
  const reason = new Error(`ghostbus server exited (${detail})`);
  reason.code = 'child-exit';
  abortWaits(reason);
  // Give the already-queued JSON-RPC responses a moment to reach the client.
  setTimeout(() => process.exit(exitCode), 250).unref();
}

function startChild() {
  try {
    child = spawn(childArgv[0], childArgv.slice(1), {
      stdio: ['pipe', 'pipe', 'pipe'],
      env: process.env,
    });
  } catch (err) {
    log(`failed to spawn child: ${err.message}`);
    handleChildGone(err.message);
    return;
  }

  child.on('error', (err) => handleChildGone(err.message, 127));
  child.on('close', (code, signal) =>
    handleChildGone(signal ? `signal ${signal}` : `code ${code}`, code ?? 1)
  );

  if (child.stdin) {
    child.stdin.on('error', (err) => {
      log(`child stdin error: ${err.message}`);
    });
  }
  if (child.stdout) {
    const childLines = readline.createInterface({
      input: child.stdout,
      crlfDelay: Infinity,
    });
    childLines.on('line', (line) => {
      if (!line.trim()) return;
      let message = null;
      try {
        message = JSON.parse(line);
      } catch {
        log('child emitted a non-JSON line; forwarding it unchanged');
        safeWrite(process.stdout, `${line}\n`);
        return;
      }
      if (
        message &&
        !Array.isArray(message) &&
        Object.prototype.hasOwnProperty.call(message, 'id')
      ) {
        const key = idKey(message.id);
        const entry = pending.get(key);
        if (entry) {
          pending.delete(key);
          if (
            entry.method === 'tools/list' &&
            message.result &&
            Array.isArray(message.result.tools) &&
            !message.result.tools.some((tool) => tool && tool.name === 'bus_wait')
          ) {
            message.result.tools.push(BUS_WAIT_TOOL);
            safeWrite(process.stdout, `${JSON.stringify(message)}\n`);
            return;
          }
        }
      }
      safeWrite(process.stdout, `${line}\n`);
    });
  } else {
    log('child has no stdout');
  }
  if (child.stderr) {
    child.stderr.on('data', (chunk) => {
      safeWrite(process.stderr, chunk);
    });
  }
}

function forward(line, id) {
  if (childExited || !child || !child.stdin || child.stdin.destroyed) {
    if (id !== undefined) {
      sendError(id, -32603, 'ghostbus server is not running');
    }
    return false;
  }
  try {
    child.stdin.write(`${line}\n`);
    return true;
  } catch (err) {
    if (id !== undefined) {
      sendError(id, -32603, `cannot reach ghostbus server: ${err.message}`);
    } else {
      log(`forward failed: ${err.message}`);
    }
    return false;
  }
}

async function runBusWait(id, params) {
  const key = idKey(id);
  const controller = new AbortController();
  const start = Date.now();
  activeWaits.set(key, controller);
  try {
    const opts = parseWaitArguments(params && params.arguments);
    const outcome = await waitForBusMessage(opts, controller.signal);
    sendResult(
      id,
      toolResult({
        agent: opts.agent,
        timedOut: outcome.timedOut,
        timeoutMs: opts.timeoutMs,
        waitedMs: Date.now() - start,
        messages: outcome.messages,
        count: outcome.messages.length,
        headSeq: outcome.headSeq,
        afterId: outcome.afterId,
        sinceSeq: opts.sinceSeq,
        unreadOnly: opts.unreadOnly,
        note: outcome.timedOut
          ? 'No new direct or broadcast message for this agent before the timeout. Messages already in the inbox are not consumed; use bus_inbox for those. Call bus_wait again with sinceSeq=headSeq or afterId to keep waiting.'
          : 'These messages were returned without being marked read. Call bus_inbox to read them and update readBy.',
      })
    );
  } catch (err) {
    const reason = controller.signal.aborted ? controller.signal.reason : null;
    if (reason && reason.code === 'cancelled') {
      sendError(id, -32800, 'Request cancelled');
    } else if (reason && reason.code === 'child-exit') {
      sendError(id, -32603, reason.message);
    } else {
      sendToolError(id, err.message);
    }
  } finally {
    activeWaits.delete(key);
  }
}

function handleClientMessage(message, line) {
  if (Array.isArray(message)) {
    // MCP stdio has no batches. If a client sends one anyway, keep it intact
    // rather than guessing at its response shape.
    forward(line, undefined);
    return;
  }
  if (!message || typeof message !== 'object') return;

  const hasId = Object.prototype.hasOwnProperty.call(message, 'id');
  const method = typeof message.method === 'string' ? message.method : null;

  if (method === 'tools/call' && message.params && message.params.name === 'bus_wait' && hasId) {
    void runBusWait(message.id, message.params);
    return;
  }

  if (method === 'notifications/cancelled') {
    const requestId = message.params && message.params.requestId;
    if (requestId !== undefined) {
      const controller = activeWaits.get(idKey(requestId));
      if (controller) {
        const reason = new Error('Request cancelled');
        reason.code = 'cancelled';
        controller.abort(reason);
      }
    }
    // ghostbus ignores this notification; forward it for transparency.
    forward(line, undefined);
    return;
  }

  if (method === null) {
    // A response from the client to a server-initiated request. Ghostbus never
    // sends one, but forwarding is harmless and keeps the proxy transparent.
    forward(line, undefined);
    return;
  }

  if (forward(line, hasId ? message.id : undefined) && hasId) {
    pending.set(idKey(message.id), { id: message.id, method });
  }
}

// ---------------------------------------------------------------------------
// Parent stdio
// ---------------------------------------------------------------------------
startChild();

process.stdout.on('error', (err) => {
  if (err && err.code === 'EPIPE') process.exit(0);
  log(`stdout error: ${err && err.message ? err.message : String(err)}`);
});
process.stderr.on('error', () => {
  // Nothing can be reported anywhere else.
});

const clientLines = readline.createInterface({
  input: process.stdin,
  crlfDelay: Infinity,
});

clientLines.on('line', (line) => {
  if (!line.trim()) return;
  let message;
  try {
    message = JSON.parse(line);
  } catch {
    // A malformed frame must not take the bus down. Answer with the JSON-RPC
    // parse error and stay in sync on the next line.
    sendError(null, -32700, 'Parse error');
    return;
  }
  try {
    handleClientMessage(message, line);
  } catch (err) {
    log(`error handling client message: ${err.stack || err.message}`);
  }
});

clientLines.on('close', () => {
  log('stdin closed; shutting down');
  if (child && child.stdin && !child.stdin.destroyed) {
    try {
      child.stdin.end();
    } catch {
      // child already gone
    }
  }
  const terminate = setTimeout(() => {
    try {
      if (child) child.kill('SIGTERM');
    } catch {
      // already gone
    }
  }, 500);
  terminate.unref();
  const hardExit = setTimeout(() => process.exit(0), 2_000);
  hardExit.unref();
});

for (const signal of ['SIGINT', 'SIGTERM']) {
  process.on(signal, () => {
    log(`received ${signal}; stopping child`);
    try {
      if (child) child.kill(signal);
    } catch {
      // already gone
    }
    setTimeout(() => process.exit(0), 2_000).unref();
  });
}
