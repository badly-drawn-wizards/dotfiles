{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    nethack
    steam
    lutris
    xboxdrv

    winetricks
    wine64Packages.waylandFull

    minecraft
  ];

  home.file = {
    ".nethackrc".text = ''
    OPTIONS=windowtype:curses, color, guicolor, clicklook
    '';
  };

}
