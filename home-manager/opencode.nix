# OpenCode's client config, declared here instead of hand-edited in
# ~/.config/opencode. Only machine-wide servers belong here; workspace-specific
# servers and skills live in /workspace/opencode.json (discovered above the
# checkout and its worktrees).
#
# `service.json` (the server's own password) is runtime state and stays unmanaged.
{ pkgs, ... }:
let
  settings = import ./opencode/settings.nix;
in
{
  # The config is Nix values in ./opencode/settings.nix, rendered here. There is
  # no checked-in JSON copy to drift; the agent-bus module imports the same
  # attrset to substitute the ghostbus wrapper store paths.
  home.file.".config/opencode/opencode.json".source =
    pkgs.writeText "opencode.json" (builtins.toJSON settings.opencode);
  home.file.".config/opencode/cli.json".source =
    pkgs.writeText "cli.json" (builtins.toJSON settings.cli);

  # ghostbus's file-backed store (the agent-bus MCP): one file shared by every
  # OpenCode location on this machine. The `.keep` file makes the directory.
  home.file.".local/share/ghostbus/.keep".text = "";
}
