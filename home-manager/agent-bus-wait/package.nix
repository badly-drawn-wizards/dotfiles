# The agent-bus-wait stdio wrapper, as a Nix package.
#
# `store` is the shared ghostbus file (the same path both clients open). The
# wrapper runs the dependency-free proxy from this directory and starts the
# unmodified ghostbus stdio server as its child, with the same --store.
{ lib
, nodejs
, writeShellScriptBin
, store
,
}:
let
  proxy = "${./proxy.mjs}";
in
writeShellScriptBin "ghostbus-wait" ''
  # npx and the ghostbus script both use `#!/usr/bin/env node`; make sure the
  # node from this package is found even in a minimal MCP launch environment.
  export PATH=${lib.makeBinPath [ nodejs ]}:$PATH
  exec ${nodejs}/bin/node ${lib.escapeShellArg proxy} \
    --store ${lib.escapeShellArg store} \
    -- ${nodejs}/bin/npx -y ghostbus --store ${lib.escapeShellArg store}
''
