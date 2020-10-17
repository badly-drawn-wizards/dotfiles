{ config, lib, pkgs, ... }:

with config.theme;
{

  window-manager.startupPrograms = [
    "xrdb -merge $HOME/.Xresources"
  ];

  home.file.".Xresources".text = ''
    URxvt*scrollBar: false
    URxvt*.font: xft:Fira Code:size=8
    URxvt*depth: 32
    URxvt*background: rgba:2900/2b00/2e00/fe00

    Emacs*toolBar: 0
    Emacs*menuBar: 0

    *foreground:   #${base05}
    *background:   #${base00}
    *cursorColor:  #${base09}

    *color0:       #${base00}
    *color1:       #${base08}
    *color2:       #${base0B}
    *color3:       #${base0A}
    *color4:       #${base0D}
    *color5:       #${base0E}
    *color6:       #${base0C}
    *color7:       #${base05}

    *color8:       #${base03}
    *color9:       #${base09}
    *color10:      #${base01}
    *color11:      #${base02}
    *color12:      #${base04}
    *color13:      #${base06}
    *color14:      #${base0F}
    *color15:      #${base07}

    Xft.dpi: 96
  '';
}
