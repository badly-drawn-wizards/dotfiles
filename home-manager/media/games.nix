{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    nethack
    # lutris

    steam
    steam-run

    winetricks
    winePackages.waylandFull

    prismlauncher
  ];

  home.file = {
    ".nethackrc".text = ''
      OPTIONS=windowtype:curses, color, guicolor, clicklook
    '';
  };

}
