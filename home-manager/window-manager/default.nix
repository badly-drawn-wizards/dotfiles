{ config, lib, pkgs, ... }:

{
  imports = [
    ./sway.nix
    ./hyprland.nix
    ./waybar
    ./swaylock.nix
    ./mako.nix
    ./rofi.nix
  ];
}
