let
  sources = import /workspace/dotfiles/nix/sources.nix;
in
import sources.emacs-overlay
