# The ghostbus push-channel server, as a Nix package.
#
# `store` is the shared ghostbus file. The server only reads it and forwards
# new messages addressed to one agent; it never writes, claims or marks them
# read. Protocol flags (--mode, --agent, --channel, --allow-from) are runtime
# arguments so the same package serves both Claude Code and OpenCode.
{ lib
, nodejs
, writeShellScriptBin
, store
,
}:
writeShellScriptBin "ghostbus-channel" ''
  # MCP hosts launch servers with a minimal environment. Use this package's
  # Node explicitly; server.mjs has no dependencies beyond the standard library.
  export PATH=${lib.makeBinPath [ nodejs ]}:$PATH
  exec ${nodejs}/bin/node ${./server.mjs} \
    --store ${lib.escapeShellArg store} \
    "$@"
''
