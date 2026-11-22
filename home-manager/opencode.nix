# OpenCode's client config, declared here instead of hand-edited in
# ~/.config/opencode. Only machine-wide servers belong here; workspace-specific
# servers and skills live in /workspace/opencode.json (discovered above the
# checkout and its worktrees).
#
# `service.json` (the server's own password) is runtime state and stays unmanaged.
{ ... }:
{
  home.file.".config/opencode/opencode.json".source = ./opencode/opencode.json;
  home.file.".config/opencode/cli.json".source = ./opencode/cli.json;
}
