# Wire the agent-bus wrappers into the clients that share the ghostbus store.
#
# `ghostbus-wait` is a superset of ghostbus's stdio MCP server: the same 29
# tools, plus bus_wait. `ghostbus-channel` is the push side: it watches the same
# file and emits Claude Code channel notifications and OpenCode MCP Events.
# Both clients must launch the wrappers (Nix store paths), not bare command
# names, or they keep the pull-only server or miss the channel.
#
# OpenCode's config is the Nix attrset in ../opencode/settings.nix. This module
# imports the same attrset and overrides only the agent-bus and ghostbus-channel
# commands with the wrappers' store paths. One module owns this file: two
# modules each forcing the same home.file target would conflict.
{ config, lib, pkgs, ... }:
let
  homeDir = config.home.homeDirectory;
  ghostbusStore = "${homeDir}/.local/share/ghostbus/bus.json";
  agentBusWait = pkgs.callPackage ./package.nix { store = ghostbusStore; };
  ghostbusChannel = pkgs.callPackage ../agent-bus-channel/package.nix { store = ghostbusStore; };

  opencodeBase = (import ../opencode/settings.nix).opencode;
  servers = opencodeBase.mcp.servers;
  channelBase = servers."ghostbus-channel" or {
    type = "local";
    command = [ "ghostbus-channel" ];
  };
  opencode = opencodeBase // {
    mcp = opencodeBase.mcp // {
      servers = servers // {
        agent-bus =
          (servers."agent-bus" or { type = "local"; })
            // {
              command = [ "${agentBusWait}/bin/ghostbus-wait" ];
            };
        ghostbus-channel = channelBase // {
          command = [ "${ghostbusChannel}/bin/ghostbus-channel" ] ++ builtins.tail channelBase.command;
        };
      };
    };
  };
in
{
  # Also put the wrappers on PATH for manual checks and for any tool that wants
  # to speak MCP to the same store.
  home.packages = [
    agentBusWait
    ghostbusChannel
  ];

  # ../opencode.nix already declares this home.file target; force only the
  # source so the generated config wins without a duplicate definition.
  home.file.".config/opencode/opencode.json".source =
    lib.mkForce (pkgs.writeText "opencode.json" (builtins.toJSON opencode));
}
