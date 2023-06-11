{ config, lib, pkgs, ... }:

let
  assigns = config.wayland.windowManager.sway.config.assigns;
  rofi-startup-workspaces = lib.concatStrings (map (s: ''echo "${s}";'') (lib.attrNames assigns));
  # Adapted from
  # https://blog.sarine.nl/2014/08/03/rofi-updates.html
  rofi-workspace-bin = name: cmd:
    let
      cmd' = cmd "$@";
      script = ''
        #!/bin/sh
        if test -z $@
        then
          swaymsg -t get_workspaces | ${pkgs.jq}/bin/jq -r ".[].name" | { cat; ${rofi-startup-workspaces} } | sort | uniq
        else
          swaymsg "${cmd'}" >/dev/null
        fi
      '';
    in pkgs.writeScriptBin name script;
  rofi-workspace-cmd = name: cmd: ''${rofi-workspace-bin name cmd}/bin/${name}'';

in
{
  options = {
    programs.rofi = with lib; with types; {
      modi = mkOption {
        type = attrsOf (nullOr str);
        default = {};
      };
      modi-cmd = mkOption {
        type = functionTo str;
        default = null;
      };
    };
  };

  config = {
    programs.rofi = {
      enable = true;
      theme = "${pkgs.dracula-rofi-theme}/theme/config2.rasi";
      package = pkgs.rofi-wayland;
      font = "{config.font} ${builtins.toString config.fontSize}";
      modi = {
        "run" = null;
        # "window" = null;
        "workspace" = rofi-workspace-cmd "rofi-workspace" (ws: "workspace ${ws}");
        "move" = rofi-workspace-cmd "rofi-move" (ws: "move window to workspace ${ws}");
      };
      modi-cmd = modi: ''${config.programs.rofi.finalPackage}/bin/rofi -matching fuzzy -show ${modi}'';
      extraConfig = {
        modi = builtins.concatStringsSep "," (lib.mapAttrsToList
          (name: script:
            name + (if isNull script then "" else ":${script}")) config.programs.rofi.modi);
        kb-row-tab = "";
        kb-remove-to-eol = "";
        kb-element-next = "";
        kb-element-prev = "";
        kb-accept-entry = "Return";
        kb-mode-next = "Tab";
        kb-mode-previous = "Shift+Tab";
        kb-row-up = "Control+k";
        kb-row-down = "Control+j";
      };
    };
  };
}
