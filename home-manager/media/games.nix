{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    nethack
    lutris
    xboxdrv

    steam
    steam-run

    winetricks
    wine64Packages.waylandFull
  ];

  home.file = {
    ".nethackrc".text = ''
    OPTIONS=windowtype:curses, color, guicolor, clicklook
    '';
  };

}
