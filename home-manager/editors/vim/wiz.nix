{ config, lib, pkgs, inputs, ... }:

# Wiz in Neovim: the wiz.nvim plugin, and tower (the Wiz toolchain manager) from Nix.
# The tower package also provides the `wiz` and `grail` shims, which run the toolchain a
# project's `wiz-toolchain` file asks for (else tower's default). The Wiz toolchain itself
# is NOT installed by Nix: tower installs it under ~/.tower from the release tarballs.
let
  wizPkgs = inputs.wiz.packages.${pkgs.stdenv.hostPlatform.system};
  tower = wizPkgs.tower;
  # The profile's shim: a path that survives updates (a /nix/store path would not).
  shim = "/etc/profiles/per-user/${config.home.username}/bin/wiz";
in
{
  home.packages = [ tower ];

  # Make tower install toolchains from release tarballs, not Nix. Idempotent; no
  # toolchain is downloaded here (`tower default nightly` does that once, or the shim does
  # on first use).
  home.activation.towerSource = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run ${tower}/bin/tower set source release
  '';

  programs.nixvim = {
    extraPlugins = [ wizPkgs.wiz-nvim ];
    # Start the server through the shim by absolute path, so it works even when Neovim is
    # started with a different PATH.
    extraConfigLua = ''
      require('wiz').setup {
        lsp = { cmd = { '${shim}', 'lsp' } },
      }
    '';
  };
}
