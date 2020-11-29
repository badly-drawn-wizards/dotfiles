let
  sources = import /workspace/dotfiles/nix/sources.nix;
  emacs-overlay = import sources.emacs-overlay;
  nix-doom-emacs = import sources.nix-doom-emacs;
in
self: super: with self; {
  doom-emacs = super.callPackage nix-doom-emacs {
    doomPrivateDir = /workspace/dotfiles/doom;
    dependencyOverrides = {
      inherit (sources) emacs-overlay doom-emacs;
      emacs = emacsGcc;
    };
  };
}
