{ config, lib, pkgs, ... }:

{
  home.package = with pkgs; [
    nautilus
    ranger
  ];
}
