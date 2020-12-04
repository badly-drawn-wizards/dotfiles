{ inputs, ... }:
{
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    (import ./rot8.nix)
  ];
}
