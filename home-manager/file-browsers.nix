{ config, lib, pkgs, ... }:

{
  home.package = with pkgs; [
    gnome.nautilus
    ranger
  ];
}
