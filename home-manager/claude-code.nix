# Claude Code's machine-wide config, declared here instead of hand-edited in
# ~/.claude. The parity target is home-manager/opencode/opencode.json: the same
# MCP servers, with the same commands and the same shared agent-bus store.
#
# How the MCP servers are stored (and why ~/.claude.json is untouched):
# `claude mcp add --scope user` writes into ~/.claude.json, which also carries
# application state (startup counts, per-project approvals, local/project MCP
# scopes). Home Manager instead emits the servers into a generated personal
# plugin, ~/.claude/skills/claude-code-home-manager/.mcp.json, using
# `programs.claude-code.mcpServers`. Claude Code 2.1.157+ discovers that plugin
# from the skills/ directory on every start, so the declaration is idempotent
# and needs no activation command. `claude mcp list` shows them as
# `plugin:hm:<server>`; their LLM-facing tool names are
# `mcp__plugin_hm_<server>__<tool>`.
#
# playwright is deliberately absent below: the
# `playwright@claude-plugins-official` plugin (enabled in settings) already
# supplies it. A second copy would give every session two browser-automation
# servers. Keep the OpenCode/Claude Code parity table in ./claude-code/README.md
# in sync when either side changes.
{ config, pkgs, ... }:
let
  homeDir = config.home.homeDirectory;
  ghostbusStore = "${homeDir}/.local/share/ghostbus/bus.json";
  # The same wrapper OpenCode uses: ghostbus's 29 tools plus bus_wait. It is a
  # Nix store path, not a copy of proxy.mjs under $HOME.
  agentBusWait = pkgs.callPackage ./agent-bus-wait/package.nix {
    store = ghostbusStore;
  };
in
{
  programs.claude-code = {
    enable = true;

    # The settings file is application-adjacent: Claude Code writes
    # enabledPlugins, autoMode and modelSettings back into it. Merge the
    # declared keys into the writable file on activation instead of replacing
    # it with an immutable store symlink. Undeclared keys (notably autoMode's
    # environment scan, which contains repo-specific context) are preserved.
    # Removing a key here does not remove it from the live file; edit the file
    # once if that matters.
    mutableSettings = true;

    settings = {
      model = "opus";
      statusLine = {
        type = "command";
        command = "bash ${homeDir}/.claude/statusline-command.sh";
      };

      # Same three plugins the hand-edited file enabled. Installation state
      # (plugins/installed_plugins.json) is application state and stays
      # unmanaged; this only keeps them enabled.
      enabledPlugins = {
        "csharp-lsp@claude-plugins-official" = true;
        "typescript-lsp@claude-plugins-official" = true;
        "playwright@claude-plugins-official" = true;
      };

      effortLevel = "high";
      promptSuggestionEnabled = false;
      askUserQuestionTimeout = "never";
      preferredNotifChannel = "notifications_disabled";
      remoteControlAtStartup = true;
      inputNeededNotifEnabled = true;
      agentPushNotifEnabled = true;
      skipAutoPermissionPrompt = true;
      useAutoModeDuringPlan = true;

      # `nix run` and `npx -y` can be slow on a cold cache. OpenCode gives the
      # nixos server 180 s of startup; this is the closest Claude Code knob
      # (milliseconds, applies to every MCP server's first connection).
      env = {
        MCP_TIMEOUT = "180000";
      };
    };

    # Mirror of the three non-playwright machine-wide servers in
    # home-manager/opencode/opencode.json. No secret is stored here: the GitHub
    # server reads the token from `gh auth token` when it starts.
    mcpServers = {
      agent-bus = {
        type = "stdio";
        command = "${agentBusWait}/bin/ghostbus-wait";
        args = [ ];
      };

      github = {
        type = "stdio";
        command = "sh";
        args = [
          "-c"
          ''exec env GITHUB_TOKEN="$(gh auth token)" MCP_IDLE_TIMEOUT_MS=0 npx -y github-mcp-server-kosta''
        ];
      };

      nixos = {
        type = "stdio";
        command = "nix";
        args = [
          "run"
          "github:utensils/mcp-nixos/v3.1.1"
          "--"
        ];
      };
    };

    # Global memory: tell every Claude Code session that the machine has one
    # shared bus and where its protocol lives. This is a CLAUDE.md section in
    # the sense that Claude Code reads it as global context.
    context = ''
      # Machine-wide coordination (agent-bus / ghostbus)

      This machine has one agent bus, exposed as the `agent-bus` MCP server.
      Every OpenCode session and every Claude Code session must use the same
      file-backed store; do not start a second bus or another messaging system:

        ${ghostbusStore}

      Use the bus for coordination with other agent lanes, not for builds or
      tests (those go through the coordinators' queue).

      - At the start: `bus_register` with a stable name (`claude-<topic>` for a
        Claude Code session, the branch/worktree name for an OpenCode lane),
        then `bus_heartbeat`, then `bus_inbox` with `unreadOnly: true`.
      - Post: `bus_send` to an agent, or to `"*"` only for a real handover or
        stop-work notice. Channels: `general` or `lane/<name>`.
      - Read once at the start and after a long task; do not poll in a loop.
        `bus_wait` blocks for the next direct/broadcast message from another
        agent (default 30 s) and returns it without marking it read; follow with
        `bus_inbox` or pass `unreadOnly`.
      - Cross-lane work: `bus_create_task`, `bus_claim_task` (15-minute lease),
        `bus_complete_task`. Risky tasks are created with
        `needsApproval: true` and released with `bus_approve_task`.
      - In Claude Code the tools are namespaced, e.g.
        `mcp__plugin_hm_agent-bus__bus_register`.

      Full protocol, parity table and apply steps:
      /etc/nixos/home-manager/claude-code/README.md

      # The Wiz workspace

      When working in `/workspace/wiz-lang` or a worktree under `/workspace/wt`:
      - read `/workspace/wiz-tools/agent-preamble.txt` and `AGENTS.md` in full; they
        are the toolkit rules (zsh, one heavy job per lane, check-in, no PR, only
        `integration/*` runs CI);
      - `integration/train11` is frozen except the stated exceptions;
        `integration/train12` is the new line; create worktrees from train12, not
        `master`;
      - every build/test goes through `/workspace/wiz-tools/heavy`; no local builds
        on the controller; single probes use `/workspace/wiz-tools/light`;
      - the bus above is shared; register as `claude-<topic>`.
    '';
  };

  # The status line is a script, not JSON, so it lives beside this module and is
  # linked into place. Claude Code invokes it through `bash`, so no exec bit is
  # required, but keep it executable for manual runs.
  home.file.".claude/statusline-command.sh" = {
    source = ./claude-code/statusline-command.sh;
    executable = true;
  };

  # NOTE: the ghostbus store directory is created by the
  # home.file.".local/share/ghostbus/.keep" declaration in opencode.nix. Do not
  # declare it again here: two definitions of the same home.file target would
  # collide.
}
