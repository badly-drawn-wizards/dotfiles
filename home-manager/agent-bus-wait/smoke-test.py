#!/usr/bin/env python3
"""Smoke test for the agent-bus-wait stdio proxy.

Spins the proxy around a real ghostbus stdio child on a temporary store and
checks: initialize, a 30-tool tools/list, the existing tools, bus_wait timeout,
a direct message posted by ghostbus-cli waking bus_wait, broadcast, own-message
exclusion, sinceSeq/afterId cursors, read-state preservation, malformed frames,
and child exit.

By default it uses `npx -y ghostbus` and `npx -y -p ghostbus ghostbus-cli`, the
same commands the Nix wrapper launches. Pass --ghostbus-mcp / --ghostbus-cli to
use a cached or checkout copy instead.

Usage:
  python3 home-manager/agent-bus-wait/smoke-test.py
  python3 home-manager/agent-bus-wait/smoke-test.py \
    --proxy ./home-manager/agent-bus-wait/proxy.mjs \
    --ghostbus-mcp /path/to/ghostbus/src/mcp-server.mjs \
    --ghostbus-cli /path/to/ghostbus/src/cli.mjs
"""
import argparse
import json
import os
import select
import shutil
import subprocess
import sys
import tempfile
import time

failures = []


def check(name, condition, detail=""):
    status = "ok" if condition else "FAIL"
    print(f"[{status}] {name}{(': ' + detail) if detail else ''}")
    if not condition:
        failures.append(name)


def read_line(stream, timeout):
    deadline = time.time() + timeout
    while True:
        remaining = deadline - time.time()
        if remaining <= 0:
            return None
        ready, _, _ = select.select([stream], [], [], remaining)
        if not ready:
            return None
        line = stream.readline()
        if line == "":
            return None
        if line.strip():
            return line


class MCP:
    def __init__(self, argv, stderr_path):
        self.stderr = open(stderr_path, "w")
        self.proc = subprocess.Popen(
            argv,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=self.stderr,
            text=True,
            bufsize=1,
        )
        self.next_id = 1
        self.pending = {}

    def send_raw(self, raw):
        self.proc.stdin.write(raw + "\n")
        self.proc.stdin.flush()

    def send(self, obj):
        self.send_raw(json.dumps(obj))

    def start_request(self, method, params=None, request_id=None):
        if request_id is None:
            request_id = self.next_id
            self.next_id += 1
        self.pending[request_id] = method
        self.send(
            {
                "jsonrpc": "2.0",
                "id": request_id,
                "method": method,
                "params": params or {},
            }
        )
        return request_id

    def read_response(self, request_id, timeout=10):
        deadline = time.time() + timeout
        while time.time() < deadline:
            line = read_line(self.proc.stdout, max(0.0, deadline - time.time()))
            if line is None:
                break
            msg = json.loads(line)
            if msg.get("id") == request_id:
                self.pending.pop(request_id, None)
                return msg
        raise TimeoutError(f"no response for id {request_id}")

    def call(self, tool, arguments=None, timeout=10):
        request_id = self.start_request(
            "tools/call", {"name": tool, "arguments": arguments or {}}
        )
        return self.read_response(request_id, timeout)

    def close(self):
        try:
            self.proc.stdin.close()
        except Exception:
            pass
        try:
            self.proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait()
        self.stderr.close()


def call_text(response):
    return json.loads(response["result"]["content"][0]["text"])


def store_state(path):
    with open(path, "r", encoding="utf-8") as fh:
        return json.load(fh)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--proxy",
        default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "proxy.mjs"),
    )
    parser.add_argument("--ghostbus-mcp", default=None)
    parser.add_argument("--ghostbus-cli", default=None)
    parser.add_argument("--node", default=shutil.which("node"))
    parser.add_argument("--npx", default=shutil.which("npx"))
    args = parser.parse_args()

    if not args.node:
        print("node not found", file=sys.stderr)
        return 2

    tmp = tempfile.mkdtemp(prefix="agent-bus-wait-test-")
    store = os.path.join(tmp, "bus.json")

    if args.ghostbus_mcp:
        child_argv = [args.node, args.ghostbus_mcp, "--store", store]
    else:
        child_argv = [args.npx, "-y", "ghostbus", "--store", store]
    proxy_argv = [args.node, args.proxy, "--store", store, "--"] + child_argv

    if args.ghostbus_cli:
        cli_prefix = [args.node, args.ghostbus_cli]
    else:
        cli_prefix = [args.npx, "-y", "-p", "ghostbus", "ghostbus-cli"]

    def cli(*words):
        return subprocess.run(
            cli_prefix + ["--store", store] + list(words),
            capture_output=True,
            text=True,
            timeout=120,
        )

    print(f"proxy={args.proxy}")
    print(f"child={' '.join(child_argv)}")
    print(f"cli={' '.join(cli_prefix)}")
    print(f"store={store}")

    mcp = MCP(proxy_argv, os.path.join(tmp, "proxy.stderr.log"))
    try:
        init = mcp.read_response(
            mcp.start_request(
                "initialize",
                {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": {"name": "smoke-test", "version": "1"},
                },
            )
        )
        check(
            "initialize",
            init.get("result", {}).get("serverInfo", {}).get("name") == "ghostbus",
            json.dumps(init)[:160],
        )
        mcp.send({"jsonrpc": "2.0", "method": "notifications/initialized"})

        listing = mcp.read_response(mcp.start_request("tools/list"))
        tools = listing["result"]["tools"]
        names = {tool["name"] for tool in tools}
        check("tools/list count is 30", len(tools) == 30, f"got {len(tools)}")
        check("tools/list includes bus_wait", "bus_wait" in names)
        for extra in ("bus_inbox", "bus_send", "bus_sync", "workspace_board"):
            check(f"tools/list still has {extra}", extra in names)

        registered = mcp.call("bus_register", {"agent": "wait-agent", "role": "test"})
        check(
            "bus_register (wait-agent)",
            not registered.get("result", {}).get("isError"),
            str(registered.get("result", {}))[:120],
        )
        registered = mcp.call("bus_register", {"agent": "peer", "role": "test"})
        check("bus_register (peer)", not registered.get("result", {}).get("isError"))
        status = call_text(mcp.call("bus_status"))
        check("bus_status", status.get("server") == "ghostbus", json.dumps(status)[:120])

        started = time.time()
        timed = call_text(mcp.call("bus_wait", {"agent": "wait-agent", "timeoutMs": 300}))
        elapsed = (time.time() - started) * 1000
        check("bus_wait timeout returns timedOut", timed.get("timedOut") is True)
        check("bus_wait timeout is a normal result", not timed.get("isError"))
        check("bus_wait timeout waited ~300ms", 250 <= elapsed <= 2500, f"{elapsed:.0f}ms")
        check("bus_wait timeout has no messages", timed.get("messages") == [])

        missing_agent = mcp.call("bus_wait", {"timeoutMs": 10})
        check(
            "bus_wait rejects a missing agent",
            missing_agent.get("result", {}).get("isError") is True,
        )
        invalid_timeout = mcp.call(
            "bus_wait", {"agent": "wait-agent", "timeoutMs": -1}
        )
        check(
            "bus_wait rejects a negative timeout",
            invalid_timeout.get("result", {}).get("isError") is True,
        )

        # Direct message from a separate process (the CLI) wakes bus_wait.
        wait_id = mcp.start_request(
            "tools/call",
            {"name": "bus_wait", "arguments": {"agent": "wait-agent", "timeoutMs": 8000}},
        )
        time.sleep(0.5)
        sent = cli("send", "peer", "wait-agent", "hello from the CLI")
        check("ghostbus-cli send", sent.returncode == 0, sent.stderr.strip()[:160])
        direct = call_text(mcp.read_response(wait_id, timeout=15))
        direct_messages = direct.get("messages", [])
        check("bus_wait direct wakes", direct.get("timedOut") is False)
        check("bus_wait returns one direct message", len(direct_messages) == 1)
        if direct_messages:
            message = direct_messages[0]
            check("direct body", message.get("body") == "hello from the CLI")
            check(
                "direct from/to",
                message.get("from") == "peer" and message.get("to") == "wait-agent",
            )
            state = store_state(store)
            stored = next(m for m in state["messages"] if m["id"] == message["id"])
            check(
                "bus_wait did not mark the message read",
                "wait-agent" not in stored.get("readBy", []),
                str(stored.get("readBy")),
            )

        inbox = call_text(
            mcp.call("bus_inbox", {"agent": "wait-agent", "unreadOnly": True})
        )
        inbox_messages = inbox.get("messages", [])
        check(
            "bus_inbox sees the unread message after bus_wait",
            any(m.get("body") == "hello from the CLI" for m in inbox_messages),
        )
        state = store_state(store)
        stored = next(m for m in state["messages"] if m["body"] == "hello from the CLI")
        check("bus_inbox marked it read", "wait-agent" in stored.get("readBy", []))

        # unreadOnly must skip the now-read direct message.
        unread = call_text(
            mcp.call(
                "bus_wait",
                {
                    "agent": "wait-agent",
                    "timeoutMs": 250,
                    "afterId": 0,
                    "unreadOnly": True,
                },
            )
        )
        check(
            "unreadOnly skips already-read messages",
            unread.get("timedOut") is True and unread.get("messages") == [],
            json.dumps(unread)[:160],
        )

        own = mcp.call(
            "bus_send",
            {
                "agent": "wait-agent",
                "to": "wait-agent",
                "body": "my own message",
                "channel": "lane/test",
            },
        )
        check("bus_send own message", not own.get("result", {}).get("isError"))
        timed = call_text(mcp.call("bus_wait", {"agent": "wait-agent", "timeoutMs": 300}))
        check("own message does not wake bus_wait", timed.get("timedOut") is True)

        head_seq = store_state(store)["seq"]
        wait_id = mcp.start_request(
            "tools/call",
            {
                "name": "bus_wait",
                "arguments": {
                    "agent": "wait-agent",
                    "timeoutMs": 8000,
                    "sinceSeq": head_seq,
                },
            },
        )
        time.sleep(0.3)
        sent = cli("send", "peer", "*", "broadcast to everyone")
        check("ghostbus-cli broadcast", sent.returncode == 0, sent.stderr.strip()[:160])
        broadcast = call_text(mcp.read_response(wait_id, timeout=15))
        broadcast_messages = broadcast.get("messages", [])
        check("bus_wait broadcast wakes", broadcast.get("timedOut") is False)
        check(
            "broadcast is to '*'",
            bool(broadcast_messages) and broadcast_messages[0].get("to") == "*",
        )
        check(
            "sinceSeq result reports the event cursor",
            broadcast.get("sinceSeq") == head_seq,
            str(broadcast.get("sinceSeq")),
        )

        state = store_state(store)
        max_id = max(m["id"] for m in state["messages"])
        catch_up = call_text(
            mcp.call(
                "bus_wait",
                {"agent": "wait-agent", "timeoutMs": 1000, "afterId": 0},
            )
        )
        check("afterId=0 catches up immediately", catch_up.get("timedOut") is False)
        check(
            "afterId=0 returns only ids > 0",
            all(m["id"] > 0 for m in catch_up.get("messages", [])),
        )
        at_head = call_text(
            mcp.call(
                "bus_wait",
                {"agent": "wait-agent", "timeoutMs": 250, "afterId": max_id},
            )
        )
        check("afterId=max id times out", at_head.get("timedOut") is True)

        mcp.send_raw("this is not json")
        parse_error = None
        deadline = time.time() + 3
        while time.time() < deadline:
            line = read_line(mcp.proc.stdout, 1)
            if line is None:
                break
            msg = json.loads(line)
            if msg.get("error", {}).get("code") == -32700:
                parse_error = msg
                break
        check("malformed frame returns JSON-RPC parse error", parse_error is not None)
        ping = mcp.read_response(mcp.start_request("ping"))
        check("proxy survives a malformed frame (ping)", ping.get("result") == {})
        listing = mcp.read_response(mcp.start_request("tools/list"))
        check(
            "tools/list after malformed frame still has bus_wait",
            any(t["name"] == "bus_wait" for t in listing["result"]["tools"]),
        )
    finally:
        mcp.close()

    # Child exit must produce an error, not a hang.
    tmp2 = tempfile.mkdtemp(prefix="agent-bus-wait-exit-")
    dead = MCP(
        [
            args.node,
            args.proxy,
            "--store",
            os.path.join(tmp2, "bus.json"),
            "--",
            args.node,
            "-e",
            "process.exit(7)",
        ],
        os.path.join(tmp2, "proxy.stderr.log"),
    )
    try:
        response = dead.read_response(
            dead.start_request(
                "initialize",
                {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": {"name": "smoke-test", "version": "1"},
                },
            ),
            timeout=5,
        )
        check(
            "child exit returns an internal error",
            response.get("error", {}).get("code") == -32603,
            json.dumps(response)[:160],
        )
    except TimeoutError as exc:
        check("child exit returns an internal error", False, str(exc))
    finally:
        dead.close()

    print()
    if failures:
        print(f"{len(failures)} FAILED: {', '.join(failures)}")
        return 1
    print("all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
