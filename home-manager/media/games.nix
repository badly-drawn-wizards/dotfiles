{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    nethack
    lutris
    xboxdrv

    steam
    steam-run

    winetricks
    winePackages.waylandFull
  ];

  home.file = {
    ".nethackrc".text = ''
    OPTIONS=windowtype:curses, color, guicolor, clicklook
    '';
  };

}
