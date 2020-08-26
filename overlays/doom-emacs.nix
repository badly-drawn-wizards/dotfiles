let
  sources = import /workspace/dotfiles/nix/sources.nix;
  nix-doom-emacs = import sources.nix-doom-emacs;
in
self: super: {
  doom-emacs = super.callPackage nix-doom-emacs {
    doomPrivateDir = /workspace/dotfiles/doom;
    dependencyOverrides = {
      inherit (sources) emacs-overlay doom-emacs;
    };
  };
}
