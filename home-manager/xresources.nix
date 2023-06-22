{ config, lib, pkgs, ... }:
with config.theme;
{

  windowManager.startupPrograms = [
    "xrdb -merge $HOME/.Xresources"
  ];

  xresources.properties = {
    "URxvt*scrollBar" = false;
    "URxvt*.font" = "xft:${config.font}:size=${builtins.toString config.fontSize}";
    "URxvt*depth" = 32;
    "URxvt*background" = "rgba:2900/2b00/2e00/fe00";

    "Emacs*toolBar" = 0;
    "Emacs*menuBar" = 0;

    "*foreground" = "  #${foreground}";
    "*background" = "  #${background}";
    "*cursorColor" = " #${cursor}";

    "*color0" = "#${color0}";
    "*color1" = "#${color1}";
    "*color2" = "#${color2}";
    "*color3" = "#${color3}";
    "*color4" = "#${color4}";
    "*color5" = "#${color5}";
    "*color6" = "#${color6}";
    "*color7" = "#${color7}";

    "*color8" = "#${color8}";
    "*color9" = "#${color9}";
    "*color10" = "#${color10}";
    "*color11" = "#${color11}";
    "*color12" = "#${color12}";
    "*color13" = "#${color13}";
    "*color14" = "#${color14}";
    "*color15" = "#${color15}";

    # "Xft.dpi" = 96;
  };
}
