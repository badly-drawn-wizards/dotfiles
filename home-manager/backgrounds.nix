{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
  set-background = pkgs.writeScriptBin "set-background" ''
    #!/usr/bin/env sh
    set -e
    BGS=$(find "${config.backgrounds.path}" -type f ! -iname ".*" -printf '%f\n' | sort)
    if [ $# -eq 0 ]
    then
      BG=$(echo -e "random\n$BGS" | ${config.programs.rofi.cmd.dmenu "background"})
    else
      BG=$1
    fi
    if [ $BG == "random" ]
    then
      BG=$(echo "$BGS" | shuf -n1)
    fi
    [ ! $BG == .current ] && ( cd ${config.backgrounds.path}; ln -sf $BG .current )
    pkill swaybg || true
    [ -f ${config.backgrounds.current} ] && ${pkgs.swaybg}/bin/swaybg -c "#${config.theme.color0}" -i "${config.backgrounds.current}"
  '';
in
{
  options = with lib; with types; {
    backgrounds = {
      path = mkOption {
        type = str;
        default = "${config.home.homeDirectory}/backgrounds";
      };
      current = mkOption {
        type = path;
        default = "${config.backgrounds.path}/.current";
      };
      set-background = mkOption {
        type = package;
        readOnly = true;
        default = set-background;
      };
    };
  };
  config = {
    windowManager = {
      extraBinds = {
        "${mod}+b" = "exec ${config.backgrounds.set-background}/bin/set-background";
      };
      startupPrograms = [
        "${config.backgrounds.set-background}/bin/set-background .current"
      ];
    };

    home.packages = [ config.backgrounds.set-background ];
  };
}
