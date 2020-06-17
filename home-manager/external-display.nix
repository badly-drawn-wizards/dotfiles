{ pkgs
, lib
, ...
}:
let
  # cvt 2560 1440 144 | tail -n1 | cut -d" " -f2- > modeline.cfg 
  modeline = builtins.readFile ./modeline.cfg;
  modename = builtins.head (lib.splitString " " modeline);
in
  pkgs.writeScriptBin 
    "configure-external-display"
    ''
      xrandr --output DP-1 --off
      xrandr --delmode DP-1 ${modename}
      xrandr --delmode DP-2 ${modename}
      xrandr --rmmode ${modename}
      xrandr --newmode ${modeline}
      xrandr --addmode DP-1 ${modename}
      xrandr --addmode DP-2 ${modename}
      xrandr --output eDP-1 --mode 3840x2160
      xrandr --output DP-1 --mode ${modename} --left-of eDP-1
    ''
