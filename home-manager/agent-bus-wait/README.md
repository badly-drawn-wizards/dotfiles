# agent-bus-wait

A dependency-free stdio MCP proxy around the third-party ghostbus server. It
keeps the bus pull-only for everything except one new tool, `bus_wait`, so an
OpenCode or Claude Code lane can block on the next inbox-relevant message
instead of polling `bus_inbox`.

The upstream package is ghostbus 0.5.12 (`npx -y ghostbus`), whose stdio server
implements only `initialize`, `tools/list` and `tools/call`. It sends no MCP
notifications. The package's HTTP relay has `/api/wait` and `/api/stream`, but
that process is not running here; the shared state is the file store
`~/.local/share/ghostbus/bus.json`.

The push side lives in `../agent-bus-channel/`: the same file watcher pattern,
but it emits Claude Code channel notifications and OpenCode 2.4 MCP Events
instead of a tool result. This proxy stays the pull side (`bus_wait`, then
`bus_inbox`).

## Layout

| File | Purpose |
| --- | --- |
| `proxy.mjs` | The stdio proxy and the local `bus_wait` implementation. |
| `package.nix` | Wraps `proxy.mjs` as `ghostbus-wait` and gives it the shared store path. |
| `module.nix` | Home Manager module: adds both bus wrappers (`ghostbus-wait`, `ghostbus-channel`) to `home.packages` and forces OpenCode's generated `agent-bus` and `ghostbus-channel` commands to their Nix store paths. |
| `README.md` | This document. |

Both clients launch the same wrapper with no arguments:

```
/nix/store/...-ghostbus-wait/bin/ghostbus-wait
```

The wrapper runs:

```
node /nix/store/...-proxy.mjs \
  --store /home/reuben/.local/share/ghostbus/bus.json \
  -- npx -y ghostbus --store /home/reuben/.local/share/ghostbus/bus.json
```

The child is the unmodified ghostbus stdio server with the same store both
clients already used. The wrapper's path is a Nix store file; nothing is copied
into `$HOME`.

## Protocol

The proxy reads newline-delimited JSON-RPC on stdin and forwards every request,
response and notification to/from the child unchanged, with two exceptions:

1. `tools/list` responses gain one tool, `bus_wait` (the 29 upstream tools are
   untouched; the list becomes 30).
2. `tools/call` for `bus_wait` is handled locally and is never sent to the
   child.

Everything else, including `initialize`, `notifications/initialized`, `ping`,
`tools/call` for any upstream tool, and malformed frames, behaves as it did
with `npx -y ghostbus`. A malformed client frame gets a JSON-RPC parse error
(`-32700`) and the proxy keeps reading. A malformed child line is forwarded
unchanged and does not crash the proxy. If the child exits, pending requests
get a JSON-RPC internal error (`-32603`) and active waits are aborted.

### `bus_wait`

```json
{
  "agent": "lane-name",
  "timeoutMs": 30000,
  "sinceSeq": 123,
  "afterId": 456,
  "unreadOnly": false
}
```

- `agent` (required): the registered name. A message is relevant when
  `to === agent` or `to === "*"`, and `from !== agent`; own sends never wake the
  caller.
- `timeoutMs`: default 30000, clamped to 600000. A timeout is a normal result,
  not `isError`: `{ "timedOut": true, "messages": [], ... }`.
- `sinceSeq`: only consider messages whose `message.send` event sequence is
  greater than this. Use `headSeq` from a previous `bus_wait` / `bus_sync`
  result, or the highest event `seq` from `bus_events`. This is the catch-up
  cursor.
- `afterId`: only consider messages with `id` greater than this. Use the last
  message id you saw. Both cursors apply when both are given.
- `unreadOnly`: default false; when true, skip messages already in the agent's
  `readBy`. A newly arrived message is normally unread anyway.

With neither cursor, the proxy snapshots the store's current message head at
the start of the call and waits for messages after it. On success the result
contains the full message objects, `count`, `headSeq`, and `afterId` set to the
last returned message id. Messages are **not** marked read; call `bus_inbox`
when you want to consume them. The result says so in its `note` field.

The implementation reads the store directly. `FileStore.save()` replaces the
file by atomic rename, so the watcher is attached to the **directory** (a watch
on the old inode would go stale). It uses `fs.watch` for immediate wakeups and
a 1-second timer as the fallback poll; it never busy-loops. Reads tolerate
`ENOENT` and partial/old JSON by retrying on the next event, so a write in
progress cannot crash a waiter.

## Wiring

- OpenCode: `home-manager/opencode.nix` declares
  `~/.config/opencode/opencode.json` from `home-manager/opencode/settings.nix`.
  `module.nix` forces only that file's `source` to a generated copy whose
  `mcp.servers.agent-bus.command` is
  `[ "/nix/store/...-ghostbus-wait/bin/ghostbus-wait" ]` and whose
  `mcp.servers.ghostbus-channel.command` is the `ghostbus-channel` wrapper
  store path followed by `--mode opencode --agent opencode`. All other OpenCode
  settings come from the checked-in JSON unchanged.
- Claude Code: `home-manager/claude-code.nix` declares
  `programs.claude-code.mcpServers.agent-bus.command` as the same wrapper store
  path, with an empty `args` list. It also declares `ghostbus-channel` with
  `--mode claude --agent claude-code`; that one is only a channel in a session
  started with `--dangerously-load-development-channels`. The generated personal
  plugin (`plugin:hm:agent-bus`, `plugin:hm:ghostbus-channel`) is discovered on
  the next start.

There is still exactly one bus: the same `bus.json` for both clients. Do not
run the HTTP relay or start a second store.

## Validate without switching

From `/etc/nixos`:

```bash
# 1. Syntax and the proxy's own stdio handshake/behavior. The child is the
#    same `npx -y ghostbus` the wrapper launches; the store is a throwaway
#    file (the test never touches the shared store).
node --check home-manager/agent-bus-wait/proxy.mjs
python3 home-manager/agent-bus-wait/smoke-test.py

# To skip npx and use a cached ghostbus checkout instead:
# python3 home-manager/agent-bus-wait/smoke-test.py \
#   --ghostbus-mcp "$HOME/.npm/_npx/664a3377955ec587/node_modules/ghostbus/src/mcp-server.mjs" \
#   --ghostbus-cli "$HOME/.npm/_npx/664a3377955ec587/node_modules/ghostbus/src/cli.mjs"

# 2. Build the home-manager generation only (no switch).
nix build -o /tmp/opencode/result-hm \
  .#nixosConfigurations.noobnoob.config.home-manager.users.reuben.home.activationPackage

# 3. Inspect what both clients will launch.
jq -r '.mcp.servers["agent-bus"].command[0]' \
  /tmp/opencode/result-hm/home-files/.config/opencode/opencode.json
jq -r '.mcpServers["agent-bus"].command' \
  /tmp/opencode/result-hm/home-files/.claude/skills/claude-code-home-manager/.mcp.json

# 4. Optional: the wrapper itself speaks MCP to its store. A timeout on a fake
#    agent is read-only and is a normal result.
printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' \
  '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}' \
  '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"bus_wait","arguments":{"agent":"nobody","timeoutMs":100}}}' |
  /tmp/opencode/result-hm/home-path/bin/ghostbus-wait 2>/tmp/opencode/wrapper.stderr | \
  jq -c 'if .id == 2 then {id, tools: (.result.tools | length), hasBusWait: (.result.tools | map(.name) | index("bus_wait") != null)} elif .id == 3 then {id, result: (.result.content[0].text | fromjson | {timedOut, count})} else . end'
```

The smoke test checks: initialize, a 30-tool `tools/list`, the existing
`bus_register`/`bus_status`/`bus_send`/`bus_inbox` tools, timeout behavior, a
direct message sent by `ghostbus-cli` waking `bus_wait`, broadcast, own-message
exclusion, `sinceSeq`/`afterId`, no read-state mutation, malformed frames, and
child exit.

To apply:

```bash
sudo nixos-rebuild switch --flake .#noobnoob
```

Restart OpenCode / Claude Code sessions afterwards so they reconnect to the new
`agent-bus` command. The store and its history are unchanged.
