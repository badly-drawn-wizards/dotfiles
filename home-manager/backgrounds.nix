{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
  set-background = pkgs.writeScriptBin "set-background" ''
  #!/bin/sh
  set -e
  BGS=$(find "${config.backgrounds}" -type f ! -iname ".*" -printf '%f\n' | sort)
  if [ $# -eq 0 ]
  then
    BG=$(echo -e "random\n$BGS" | rofi -dmenu -p "background")
  elif [ $1 == "last" ]
  then
    BG=$(cat ${config.backgrounds}/.last)
  else
    BG=$1
  fi
  if [ $BG == "random" ]
  then
    BG=$(echo "$BGS" | shuf -n1)
  fi
  echo $BG > ${config.backgrounds}/.last
  pkill swaybg || true
  ${pkgs.swaybg}/bin/swaybg -i "${config.backgrounds}/$BG"
  '';
in
{
  options = with lib; with types; {
    backgrounds = mkOption {
      type = str;
    };
  };
  config = {
    backgrounds = "${config.home.homeDirectory}/backgrounds";
    windowManager = {
      extraBinds = {
        "${mod}+b" = "exec ${set-background}/bin/set-background";
      };
      startupPrograms = [
        "${set-background}/bin/set-background last"
      ];
    };

    home.packages = [ set-background ];
  };
}
