{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    steam
    lutris
    xboxdrv

    winetricks
    wine64Packages.waylandFull
  ];

}
