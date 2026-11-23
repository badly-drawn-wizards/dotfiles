# OpenCode client config

`home-manager/opencode.nix` links these files into `~/.config/opencode/`:

| File | Purpose |
| --- | --- |
| `opencode.json` | Machine-wide MCP servers, permissions, plugins. Workspace-specific servers stay in `/workspace/opencode.json`. |
| `cli.json` | CLI attention/plugin settings. |
| `../agent-bus-wait/module.nix` | Generates `opencode.json` with the `agent-bus` and `ghostbus-channel` commands replaced by the Nix store paths of the wrappers. |

## `agent-bus`

The checked-in `opencode.json` names the command `ghostbus-wait`. At build
time, `home-manager/agent-bus-wait/module.nix` forces only the `source` of
`~/.config/opencode/opencode.json` to a generated copy and replaces that one
command with the wrapper's absolute Nix store path:

```json
"agent-bus": {
  "type": "local",
  "command": ["/nix/store/...-ghostbus-wait/bin/ghostbus-wait"],
  "timeout": { "startup": 90000, "catalog": 30000, "execution": 120000 },
  "codemode": true
}
```

Every other field (permissions, plugins, github/nixos/playwright servers) comes
from the checked-in JSON unchanged. The wrapper runs the same unmodified
ghostbus stdio server with the same `--store` as Claude Code's
`plugin:hm:agent-bus` entry, and adds the local `bus_wait` tool. See
`home-manager/agent-bus-wait/README.md` for the proxy design and cursors.

`bus_wait` is a blocking, read-only tool: it returns new direct/broadcast
messages without marking them read and reports `timedOut: true` on timeout.
Use it instead of a polling loop. Keep `timeoutMs` below the server's
`execution` timeout (120000 ms).

## `ghostbus-channel`

The push side, from the same store, is declared as a second server with
`protocol: "2026-07-28"` (MCP Events requires the modern handshake) and
`--mode opencode --agent opencode`. See `../agent-bus-channel/README.md` for the
event type (`bus.message`), the `events/list` / `events/stream` contract and an
`automation.create` event-trigger example. The channel is one-way; reply with
the `agent-bus` tools.

`--agent opencode` must match the name the lane registers on the bus. A lane
that registers under its branch name needs that argument changed in
`home-manager/agent-bus-wait/module.nix` (or a per-lane config override).

## Validate and apply

```bash
cd /etc/nixos
nix build -o /tmp/opencode/result-hm \
  .#nixosConfigurations.noobnoob.config.home-manager.users.reuben.home.activationPackage
jq -r '.mcp.servers["agent-bus"].command[0]' \
  /tmp/opencode/result-hm/home-files/.config/opencode/opencode.json
jq -r '.mcp.servers["ghostbus-channel"].command' \
  /tmp/opencode/result-hm/home-files/.config/opencode/opencode.json
```

Then apply without a separate Claude Code step:

```bash
sudo nixos-rebuild switch --flake .#noobnoob
```

Restart OpenCode so it reconnects to the new `agent-bus` command. The shared
store `/home/reuben/.local/share/ghostbus/bus.json` is unchanged.
