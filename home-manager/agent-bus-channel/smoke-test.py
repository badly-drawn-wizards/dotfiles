#!/usr/bin/env python3
"""Smoke test for home-manager/agent-bus-channel/server.mjs.

Spawns the raw stdio server against a temporary ghostbus store and checks both
protocol eras without any MCP SDK:

  * Claude Code legacy initialize + notifications/claude/channel
  * OpenCode 2.4 server/discover + events/list + events/stream/subscribe

Run: python3 smoke-test.py [path/to/server.mjs]
"""
import json
import os
import queue
import select
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
SERVER = Path(sys.argv.pop(1)) if len(sys.argv) > 1 else HERE / "server.mjs"


def bus_state(messages):
    return {
        "agents": {},
        "messages": messages,
        "events": [
            {"type": "message.send", "messageId": m["id"], "seq": m["id"]}
            for m in messages
        ],
        "seq": messages[-1]["id"] if messages else 0,
        "msgSeq": messages[-1]["id"] if messages else 0,
    }


def message(mid, sender, to, body, channel="general"):
    return {
        "id": mid,
        "from": sender,
        "to": to,
        "channel": channel,
        "body": body,
        "ts": f"2026-11-23T12:00:{mid:02d}.000Z",
        "readBy": [],
    }


def write_store(path, messages):
    temp = f"{path}.tmp.{os.getpid()}"
    with open(temp, "w", encoding="utf-8") as fh:
        json.dump(bus_state(messages), fh)
    os.replace(temp, path)


def append_store(path, entry):
    with open(path, encoding="utf-8") as fh:
        state = json.load(fh)
    state["messages"].append(entry)
    state.setdefault("events", []).append(
        {"type": "message.send", "messageId": entry["id"], "seq": entry["id"]}
    )
    state["seq"] = entry["id"]
    state["msgSeq"] = entry["id"]
    temp = f"{path}.tmp.{os.getpid()}"
    with open(temp, "w", encoding="utf-8") as fh:
        json.dump(state, fh)
    os.replace(temp, path)


class RawServer:
    def __init__(self, store, mode, agent, *extra):
        self.proc = subprocess.Popen(
            [
                "node",
                str(SERVER),
                "--store",
                str(store),
                "--mode",
                mode,
                "--agent",
                agent,
                *extra,
            ],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
            env={**os.environ, "PATH": os.environ.get("PATH", "")},
        )
        self.lines = queue.Queue()
        self.ready = threading.Event()
        self.stderr_lines = []
        threading.Thread(target=self._pump_stdout, daemon=True).start()
        threading.Thread(target=self._pump_stderr, daemon=True).start()
        if not self.ready.wait(5):
            self.close()
            raise AssertionError(f"server did not become ready; stderr={self.stderr_text()}")

    def _pump_stdout(self):
        for line in self.proc.stdout:
            line = line.strip()
            if not line:
                continue
            try:
                self.lines.put(json.loads(line))
            except json.JSONDecodeError:
                self.lines.put({"raw": line})

    def _pump_stderr(self):
        for line in self.proc.stderr:
            self.stderr_lines.append(line.rstrip())
            if "ready" in line:
                self.ready.set()

    def stderr_text(self):
        return "\n".join(self.stderr_lines)

    def send(self, obj):
        self.proc.stdin.write(json.dumps(obj) + "\n")
        self.proc.stdin.flush()

    def send_raw(self, text):
        self.proc.stdin.write(text + "\n")
        self.proc.stdin.flush()

    def recv(self, timeout=5.0):
        try:
            return self.lines.get(timeout=timeout)
        except queue.Empty:
            raise AssertionError(f"timed out waiting for a server message; stderr={self.stderr_text()}")

    def recv_until(self, predicate, timeout=5.0):
        deadline = time.monotonic() + timeout
        seen = []
        while time.monotonic() < deadline:
            remaining = max(0.0, deadline - time.monotonic())
            try:
                item = self.lines.get(timeout=min(remaining, 0.25))
            except queue.Empty:
                continue
            seen.append(item)
            if predicate(item):
                return item
        raise AssertionError(
            f"no matching message; saw {json.dumps(seen)[:2000]}; stderr={self.stderr_text()}"
        )

    def recv_none(self, duration=0.8, ignore=None):
        deadline = time.monotonic() + duration
        seen = []
        while time.monotonic() < deadline:
            try:
                item = self.lines.get(timeout=min(0.1, max(0.0, deadline - time.monotonic())))
            except queue.Empty:
                continue
            if ignore is not None and ignore(item):
                continue
            seen.append(item)
        return seen

    def close(self):
        try:
            if self.proc.stdin:
                self.proc.stdin.close()
        except Exception:
            pass
        try:
            self.proc.terminate()
            self.proc.wait(timeout=3)
        except Exception:
            self.proc.kill()
        for pipe in (self.proc.stdout, self.proc.stderr):
            try:
                if pipe:
                    pipe.close()
            except Exception:
                pass


def request(request_id, method, params=None):
    obj = {"jsonrpc": "2.0", "id": request_id, "method": method}
    if params is not None:
        obj["params"] = params
    return obj


class ChannelTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="ghostbus-channel-")
        self.store = Path(self.tmp.name) / "bus.json"
        self.servers = []

    def tearDown(self):
        for server in self.servers:
            server.close()
        self.tmp.cleanup()

    def start(self, mode, agent, *extra):
        server = RawServer(self.store, mode, agent, *extra)
        self.servers.append(server)
        return server

    def test_claude_legacy_channel(self):
        write_store(
            self.store,
            [message(1, "lane-old", "claude-code", "before startup, must not replay")],
        )
        server = self.start(
            "claude", "claude-code", "--allow-from", "lane-a,lane-b"
        )
        server.send(
            request(
                1,
                "initialize",
                {
                    "protocolVersion": "2025-11-25",
                    "capabilities": {},
                    "clientInfo": {"name": "smoke", "version": "1"},
                },
            )
        )
        init = server.recv_until(lambda m: m.get("id") == 1)
        capabilities = init["result"]["capabilities"]
        self.assertEqual(capabilities["experimental"]["claude/channel"], {})
        self.assertNotIn("claude/channel/permission", capabilities["experimental"])
        self.assertEqual(init["result"]["serverInfo"]["name"], "ghostbus-channel")
        server.send({"jsonrpc": "2.0", "method": "notifications/initialized"})

        # The old message must not be replayed.
        self.assertEqual(server.recv_none(0.5), [])

        append_store(self.store, message(2, "lane-a", "claude-code", "hello claude"))
        notification = server.recv_until(
            lambda m: m.get("method") == "notifications/claude/channel"
        )
        params = notification["params"]
        self.assertIn("UNTRUSTED", params["content"])
        self.assertIn("hello claude", params["content"])
        self.assertEqual(params["meta"]["trust"], "untrusted")
        self.assertEqual(params["meta"]["user"], "lane-a")
        self.assertEqual(params["meta"]["message_id"], "2")
        self.assertEqual(params["meta"]["chat_id"], "general")

        # A message this agent sent itself must not wake it.
        append_store(self.store, message(3, "claude-code", "claude-code", "own echo"))
        self.assertEqual(server.recv_none(0.8), [])

        # Broadcast from an allowlisted sender is delivered.
        append_store(self.store, message(4, "lane-b", "*", "broadcast"))
        broadcast = server.recv_until(
            lambda m: m.get("method") == "notifications/claude/channel"
        )
        self.assertIn("broadcast", broadcast["params"]["content"])
        self.assertEqual(broadcast["params"]["meta"]["to"], "*")

        # A sender outside --allow-from is dropped.
        append_store(self.store, message(5, "lane-evil", "claude-code", "drop me"))
        self.assertEqual(server.recv_none(0.8), [])

        # A malformed frame gets a parse error and the server stays alive.
        server.send_raw("{not json")
        parse_error = server.recv_until(lambda m: m.get("error", {}).get("code") == -32700)
        self.assertIsNone(parse_error["id"])
        append_store(self.store, message(6, "lane-a", "claude-code", "still alive"))
        server.recv_until(
            lambda m: m.get("method") == "notifications/claude/channel"
            and "still alive" in m["params"]["content"]
        )

    def test_claude_caps_modern_revision(self):
        """Claude's v2 runtime asks for 2026-07-28; the channel must stay legacy."""
        write_store(self.store, [])
        server = self.start("claude", "claude-code")
        server.send(
            request(
                1,
                "initialize",
                {
                    "protocolVersion": "2026-07-28",
                    "capabilities": {},
                    "clientInfo": {"name": "smoke-v2", "version": "1"},
                },
            )
        )
        init = server.recv_until(lambda m: m.get("id") == 1)["result"]
        self.assertEqual(init["protocolVersion"], "2025-11-25")
        self.assertEqual(init["capabilities"]["experimental"]["claude/channel"], {})
        self.assertNotIn("claude/channel/permission", init["capabilities"]["experimental"])

    def test_claude_discover_advertises_no_modern(self):
        """v2 skips the modern probe when claude mode advertises no revision."""
        write_store(self.store, [])
        server = self.start("claude", "claude-code")
        server.send(request(1, "server/discover", {}))
        discover = server.recv_until(lambda m: m.get("id") == 1)["result"]
        self.assertEqual(discover["supportedVersions"], [])

    def test_opencode_modern_events(self):
        write_store(self.store, [message(1, "lane-old", "opencode", "old")])
        server = self.start("opencode", "opencode", "--heartbeat-ms", "200")

        server.send(
            request(
                0,
                "initialize",
                {
                    "protocolVersion": "2026-07-28",
                    "capabilities": {},
                    "clientInfo": {"name": "smoke", "version": "1"},
                },
            )
        )
        init = server.recv_until(lambda m: m.get("id") == 0)["result"]
        self.assertEqual(init["protocolVersion"], "2026-07-28")

        server.send(
            request(
                1,
                "server/discover",
                {
                    "_meta": {
                        "io.modelcontextprotocol/protocolVersion": "2026-07-28",
                        "io.modelcontextprotocol/clientInfo": {
                            "name": "smoke",
                            "version": "1",
                        },
                        "io.modelcontextprotocol/clientCapabilities": {},
                    }
                },
            )
        )
        discover = server.recv_until(lambda m: m.get("id") == 1)["result"]
        self.assertEqual(discover["resultType"], "complete")
        self.assertIn("2026-07-28", discover["supportedVersions"])
        self.assertIn(
            "io.modelcontextprotocol/events", discover["capabilities"]["extensions"]
        )
        self.assertNotIn("claude/channel", discover["capabilities"].get("extensions", {}))
        self.assertEqual(
            discover["_meta"]["io.modelcontextprotocol/serverInfo"]["name"],
            "ghostbus-channel",
        )

        server.send(request(2, "events/list", {}))
        listed = server.recv_until(lambda m: m.get("id") == 2)["result"]
        self.assertEqual(listed["resultType"], "complete")
        self.assertEqual(len(listed["events"]), 1)
        event_type = listed["events"][0]
        self.assertEqual(event_type["name"], "bus.message")
        self.assertEqual(event_type["delivery"], ["push"])
        self.assertIn("body", event_type["payloadSchema"]["properties"])

        # Open a channel-filtered stream. The request deliberately stays open.
        server.send(
            request(
                3,
                "events/stream",
                {"name": "bus.message", "arguments": {"channel": "general"}},
            )
        )
        active = server.recv_until(
            lambda m: m.get("method") == "notifications/events/active"
        )
        self.assertEqual(active["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"], 3)
        self.assertEqual(active["params"]["heartbeatMs"], 200)

        # Heartbeats keep the stream alive in OpenCode's watchdog.
        heartbeat = server.recv_until(
            lambda m: m.get("method") == "notifications/events/heartbeat"
        )
        self.assertEqual(heartbeat["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"], 3)

        append_store(self.store, message(2, "lane-a", "opencode", "event body"))
        event = server.recv_until(
            lambda m: m.get("method") == "notifications/events/event"
            and m["params"].get("eventId") == "bus-2"
        )
        self.assertEqual(event["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"], 3)
        self.assertEqual(event["params"]["name"], "bus.message")
        self.assertEqual(event["params"]["data"]["body"], "event body")
        self.assertEqual(event["params"]["data"]["trust"], "untrusted")
        self.assertEqual(event["params"]["cursor"], "2")

        # A different channel is filtered out for this stream.
        append_store(self.store, message(3, "lane-a", "opencode", "other", "lane/x"))
        self.assertEqual(
            server.recv_none(0.8, ignore=lambda m: m.get("method") == "notifications/events/heartbeat"),
            [],
        )

        # events/subscribe is accepted as the draft spelling of the same call.
        server.send(
            request(
                4,
                "events/subscribe",
                {"name": "bus.message", "arguments": {"channel": "general"}},
            )
        )
        subscribed = server.recv_until(
            lambda m: m.get("method") == "notifications/events/active"
            and m["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"] == 4
        )
        append_store(self.store, message(5, "lane-b", "*", "second stream"))
        second = server.recv_until(
            lambda m: m.get("method") == "notifications/events/event"
            and m["params"].get("eventId") == "bus-5"
            and m["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"] == 4
        )
        self.assertEqual(second["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"], 4)

        # Cancelling a stream stops its notifications. Drain queued heartbeats.
        server.send(
            {
                "jsonrpc": "2.0",
                "method": "notifications/cancelled",
                "params": {"requestId": 3},
            }
        )
        server.recv_none(0.5, ignore=lambda m: m.get("method") == "notifications/events/heartbeat")
        append_store(self.store, message(6, "lane-a", "opencode", "after cancel"))
        remaining = server.recv_none(
            0.8, ignore=lambda m: m.get("method") == "notifications/events/heartbeat"
        )
        self.assertFalse(
            any(
                m.get("method") == "notifications/events/event"
                and m.get("params", {}).get("eventId") == "bus-6"
                and m["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"] == 3
                for m in remaining
            ),
            f"cancelled stream still delivered: {remaining}",
        )
        # The other stream is still alive.
        self.assertTrue(
            any(
                m.get("method") == "notifications/events/event"
                and m.get("params", {}).get("eventId") == "bus-6"
                and m["params"]["_meta"]["io.modelcontextprotocol/subscriptionId"] == 4
                for m in remaining
            ),
            f"second stream missed the event: {remaining}",
        )


if __name__ == "__main__":
    unittest.main(verbosity=2)
