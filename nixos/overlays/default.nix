{ inputs, ... }:
{
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    inputs.nur.overlay
    (import ./rot8.nix)
    (import ./lean4-mode.nix)
  ];
}
