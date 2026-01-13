{ config, lib, pkgs, ... }:

let
  draculaTheme = {
    package = pkgs.dracula-theme;
    name = "Dracula";
  };
in
{

  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 16;
    x11 = {
      enable = true;
      defaultCursor = "Adwaita";
    };
  };
  gtk = {
    theme = draculaTheme;
    iconTheme = draculaTheme;
  };

  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };
  };
}
