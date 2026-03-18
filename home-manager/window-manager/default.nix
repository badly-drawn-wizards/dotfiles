{ config, lib, pkgs, ... }:

{
  imports = [
    ./sway.nix
    # ./hyprland.nix
    ./noctalia.nix
    ./rofi.nix
    ./kanshi.nix
  ];

  home.packages = with pkgs; [
    wdisplays
    wlr-randr
  ];
}
