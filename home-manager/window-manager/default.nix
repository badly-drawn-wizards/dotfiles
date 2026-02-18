{ config, lib, pkgs, ... }:

{
  imports = [
    ./sway.nix
    # ./hyprland.nix
    ./waybar
    ./swaylock.nix
    ./swaync.nix
    ./rofi.nix
    ./kanshi.nix
  ];

  home.packages = with pkgs; [
    wdisplays
    wlr-randr
  ];
}
