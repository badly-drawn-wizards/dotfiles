{ config, lib, pkgs, inputs, ... }:

# Wiz in Neovim: the wiz.nvim plugin, and tower (the Wiz toolchain manager) from Nix.
# The Wiz toolchain itself is NOT installed by Nix: tower installs it under ~/.tower from
# the GitHub release tarballs, and the `wiz`/`grail` shims in ~/.tower/bin pick the
# toolchain a project's `wiz-toolchain` file asks for (else tower's default).
let
  wizPkgs = inputs.wiz.packages.${pkgs.stdenv.hostPlatform.system};
  tower = wizPkgs.tower;
  # The shims call tower through the user profile, a path that survives updates and
  # garbage collection (a /nix/store path would not).
  towerBin = "/etc/profiles/per-user/${config.home.username}/bin/tower";
  shim = "${config.home.homeDirectory}/.tower/bin/wiz";
in
{
  home.packages = [ tower ];
  home.sessionPath = [ "$HOME/.tower/bin" ];

  # Write the shims and make tower install toolchains from release tarballs, not Nix.
  # Idempotent; no toolchain is downloaded here (`tower default nightly` does that once,
  # or the shim does on first use).
  home.activation.towerShims = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run ${tower}/bin/tower --self ${towerBin} self shims
    run ${tower}/bin/tower set source release
  '';

  programs.nixvim = {
    extraPlugins = [ wizPkgs.wiz-nvim ];
    # Start the server through tower's shim by absolute path, so it works even when
    # Neovim is started without ~/.tower/bin on PATH.
    extraConfigLua = ''
      require('wiz').setup {
        lsp = { cmd = { '${shim}', 'lsp' } },
      }
    '';
  };
}
