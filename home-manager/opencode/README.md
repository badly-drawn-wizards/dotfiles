# OpenCode client config

`home-manager/opencode.nix` renders the Nix values in `settings.nix` into
`~/.config/opencode/`:

| File | Purpose |
| --- | --- |
| `settings.nix` | The single source: machine-wide MCP servers, permissions, plugins, CLI attention. Workspace-specific servers stay in `/workspace/opencode.json`. |
| `opencode.json` (rendered) | `settings.nix`'s `opencode` attrset, via `builtins.toJSON`. |
| `cli.json` (rendered) | `settings.nix`'s `cli` attrset. |
| `../agent-bus-wait/module.nix` | Imports `settings.nix` and replaces only the `agent-bus` and `ghostbus-channel` commands with the wrappers' Nix store paths. |

## `agent-bus`

The base config names the command `ghostbus-wait`. At build time,
`home-manager/agent-bus-wait/module.nix` imports `settings.nix`, adds the Nix
store paths, and writes the resulting `opencode.json`:

```json
"agent-bus": {
  "type": "local",
  "command": ["/nix/store/...-ghostbus-wait/bin/ghostbus-wait"],
  "timeout": { "startup": 90000, "catalog": 30000, "execution": 120000 },
  "codemode": true
}
```

Every other field (permissions, plugins, github/nixos/playwright servers) comes
from the Nix settings unchanged. The wrapper runs the same unmodified
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
