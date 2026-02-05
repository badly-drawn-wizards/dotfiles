{ config, lib, pkgs, ... }:

let
  steam' = pkgs.steam.override {
    extraPacakges = pkgs: with pkgs; [
      gamescope
      mangohud
    ];
  };
in
{

  home.packages = with pkgs; [
    nethack
    lutris

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
