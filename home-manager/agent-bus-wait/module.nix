# Wire the agent-bus-wait proxy into the clients that share the ghostbus store.
#
# The proxy is a superset of ghostbus's stdio MCP server: the same 29 tools,
# plus bus_wait. Both clients must launch the wrapper (a Nix store path), not
# `npx -y ghostbus` directly, or they keep the pull-only server.
#
# OpenCode reads a static JSON file declared in ../opencode.nix. This module
# overrides only that file's `source` with a generated copy whose agent-bus
# command is the wrapper's store path. The rest of the JSON is read from
# ../opencode/opencode.json unchanged.
{ config, lib, pkgs, ... }:
let
  homeDir = config.home.homeDirectory;
  ghostbusStore = "${homeDir}/.local/share/ghostbus/bus.json";
  agentBusWait = pkgs.callPackage ./package.nix { store = ghostbusStore; };

  opencodeBase = builtins.fromJSON (builtins.readFile ../opencode/opencode.json);
  opencode = opencodeBase // {
    mcp = opencodeBase.mcp // {
      servers = opencodeBase.mcp.servers // {
        agent-bus =
          (opencodeBase.mcp.servers."agent-bus" or { type = "local"; })
            // {
            command = [ "${agentBusWait}/bin/ghostbus-wait" ];
          };
      };
    };
  };
in
{
  # Also puts `ghostbus-wait` on PATH for manual checks and for any tool that
  # wants to speak MCP to the same store.
  home.packages = [ agentBusWait ];

  # ../opencode.nix already declares this home.file target; force only the
  # source so the generated config wins without a duplicate definition.
  home.file.".config/opencode/opencode.json".source =
    lib.mkForce (pkgs.writeText "opencode.json" (builtins.toJSON opencode));
}
