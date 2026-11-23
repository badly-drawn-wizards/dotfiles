# Claude Code parity and the shared agent bus

This directory holds the non-Nix payload for `home-manager/claude-code.nix`:
the status-line script. The module declares Claude Code's machine-wide config so
it no longer drifts from `home-manager/opencode/opencode.json` by hand.

## What is declared where

| What | Declared in | Rendered to |
| --- | --- | --- |
| `settings.json` (model, status line, plugins, effort, env) | `programs.claude-code.settings` | merged into `~/.claude/settings.json` on activation |
| MCP servers (`agent-bus`, `github`, `nixos`) | `programs.claude-code.mcpServers` | `~/.claude/skills/claude-code-home-manager/.mcp.json` |
| Global memory / bus instructions | `programs.claude-code.context` | `~/.claude/CLAUDE.md` |
| Status-line script | `home.file.".claude/statusline-command.sh"` | `~/.claude/statusline-command.sh` |
| ghostbus store directory | `home.file.".local/share/ghostbus/.keep"` in `opencode.nix` | `~/.local/share/ghostbus/` |

`~/.claude.json` is **not** managed. It holds application state (startup counts,
project approvals, local/project-scoped MCP servers); Home Manager never writes
it.

## Parity table (machine-wide servers)

| OpenCode server | Claude Code server | Command | Notes |
| --- | --- | --- | --- |
| `playwright` | `plugin:playwright:playwright` | `npx @playwright/mcp@latest` | Provided by the `playwright@claude-plugins-official` plugin already enabled in `settings.enabledPlugins`. Deliberately **not** duplicated in `mcpServers`. |
| `github` | `plugin:hm:github` | `sh -c 'exec env GITHUB_TOKEN="$(gh auth token)" MCP_IDLE_TIMEOUT_MS=0 npx -y github-mcp-server-kosta'` | Token is read at runtime from `gh`; no secret is in the Nix store. |
| `agent-bus` | `plugin:hm:agent-bus` | `npx -y ghostbus --store /home/reuben/.local/share/ghostbus/bus.json` | Same file as every OpenCode session. |
| `nixos` | `plugin:hm:nixos` | `nix run github:utensils/mcp-nixos/v3.1.1 --` | Same pinned rev as OpenCode. |
| `wiz`, `scry` | project/local scope in `/workspace` | `sh /workspace/wiz-tools/{wiz,scry}-mcp` | Workspace-specific; they stay out of the machine-wide config. |

Claude Code names the generated plugin `hm`, so the servers appear as
`plugin:hm:<server>` and their tools are namespaced
`mcp__plugin_hm_<server>__<tool>` (for example
`mcp__plugin_hm_agent-bus__bus_register`).

## MCP storage mechanism

`claude mcp add --scope user` is the CLI-supported route, but it writes
`mcpServers` into `~/.claude.json`, the same file that carries application
state. Home Manager's `programs.claude-code.mcpServers` option writes the
servers into a generated personal plugin instead:

```
~/.claude/skills/claude-code-home-manager/.claude-plugin/plugin.json   {"name":"hm"}
~/.claude/skills/claude-code-home-manager/.mcp.json                     {"mcpServers":{...}}
```

Claude Code 2.1.157+ discovers personal plugins in `~/.claude/skills/` on every
start, so this is idempotent: no activation command has to run `claude mcp`.
The `.mcp.json` is a store symlink under `home-manager-files`, so removing a
server from the module removes it from the next generation. `claude mcp list`
shows the result; `claude mcp get plugin:hm:agent-bus` inspects one entry.

## The shared bus (ghostbus)

There is exactly one bus, and it is a file:

```
/home/reuben/.local/share/ghostbus/bus.json
```

OpenCode's `agent-bus` server and Claude Code's `plugin:hm:agent-bus` server both
open that path with `--store`. A message written by either client is visible to
the other. Do not start a second bus, and do not use the bus as a build/test
queue.

### Names

- An OpenCode lane registers under its branch/worktree name (`claude-code-parity`,
  `hdl-pkg`, ...), role `builder` and its capabilities.
- A Claude Code session registers under `claude-<topic>` (for example
  `claude-code-parity` if it owns that lane) so the coordinator can tell the
  clients apart. One name per session; re-registering keeps the same identity.

### Protocol

1. **Register** once: `bus_register({ agent, role, capabilities })`.
2. **Heartbeat**: `bus_heartbeat({ agent })` at the start and after long tasks.
3. **Read**: `bus_inbox({ agent, unreadOnly: true, limit: 50 })` at the start
   and after a long report. Do not loop/poll.
4. **Post**: `bus_send({ agent, to, channel, body })`. Use `general` for
   coordinator-visible announcements, `lane/<branch>` for lane-local chatter,
   and `to: "*"` only for a real handover or a stop-work notice.
5. **Tasks**: `bus_create_task`, `bus_claim_task` (exclusive for 15 minutes),
   `bus_complete_task`. Risky work uses `needsApproval: true`; a coordinator (or
   another authorised agent) releases it with `bus_approve_task({ agent, id })`.
   Use `bus_list_tasks` / `workspace_board` to see state.
6. **Handover**: `workspace_put_context` / `workspace_get_capsule` for durable
   shared state; `bus_events` is the provenance log.

The full tool surface is the `agent-bus` MCP server; both clients expose the same
29 tools. In Claude Code they are the `mcp__plugin_hm_agent-bus__*` namespace.

## Validate and apply

```bash
cd /etc/nixos

# Evaluate and build the home-manager generation only (no switch):
nix build -o result-hm \
  .#nixosConfigurations.noobnoob.config.home-manager.users.reuben.home.activationPackage

# Or the full system closure:
sudo nixos-rebuild build --flake .#noobnoob

# Inspect the generated plugin and settings before activating:
find result-hm/home-files/.claude -maxdepth 4 \( -type f -o -type l \) -print
cat result-hm/home-files/.claude/skills/claude-code-home-manager/.mcp.json
```

`sudo nixos-rebuild switch --flake .#noobnoob` then:

1. links `~/.claude/statusline-command.sh` and
   `~/.claude/skills/claude-code-home-manager` into the new generation;
2. merges the declared keys into the existing writable
   `~/.claude/settings.json` (its current mode is preserved; a new file gets
   mode 600), leaving `autoMode`, `modelSettings` and any other undeclared keys
   intact;
3. writes `~/.claude/CLAUDE.md` with the bus instructions;
4. **does not** touch `~/.claude.json`.

Verify after activation:

```bash
claude mcp list | grep 'plugin:hm'
claude mcp get plugin:hm:agent-bus
nix run nixpkgs#jq -- .mcpServers ~/.claude.json   # should be unchanged by the switch
```

If `plugin:hm:nixos` does not connect on the first launch, it is the cold `nix
run`; the module sets `MCP_TIMEOUT=180000` in `settings.env` for that reason.

## End-to-end bus check

From either client:

```js
bus_register({ agent: "claude-code-parity", role: "builder", capabilities: ["nixos"] })
bus_send({ agent: "claude-code-parity", to: "claude-code-parity",
           channel: "lane/claude-code-parity", body: "parity check" })
bus_inbox({ agent: "claude-code-parity", unreadOnly: true })
```

A run on 2026-11-23 registered `claude-code-parity`, posted message id `375`, and
read it back from the same store; the message remains visible to any later
session that registers and reads `lane/claude-code-parity`.
