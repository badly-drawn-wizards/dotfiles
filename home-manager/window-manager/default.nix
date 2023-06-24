{ config, lib, pkgs, ... }:

{
  imports = [
    ./sway.nix
    ./waybar
    ./swaylock.nix
    ./mako.nix
    ./rofi.nix
  ];
}
