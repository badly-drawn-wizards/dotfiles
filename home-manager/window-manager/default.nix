{ config, lib, pkgs, ... }:

{
  imports = [
    ./sway.nix
    ./hyprland.nix
    ./waybar
    ./swaylock.nix
    ./mako.nix
    ./rofi.nix
    ./kanshi.nix
  ];

  home.packages = with pkgs; [
    wdisplays
  ];
}
