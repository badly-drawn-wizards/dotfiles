{ pkgs
, lib
, ...
}:

let
  # Adapted from
  # https://blog.sarine.nl/2014/08/03/rofi-updates.html
  rofi-workspace-bin = name: cmd: 
    let
      cmd' = cmd "$@";
      script = ''
        #!/bin/sh
        if test -z $@
        then
          i3-msg -t get_workspaces | ${pkgs.jq}/bin/jq -r ".[].name"
        else
          i3-msg "${cmd'}" >/dev/null
        fi
      '';
    in pkgs.writeScriptBin name script;
  rofi-workspace-cmd = name: cmd: ''${rofi-workspace-bin name cmd}/bin/${name}'';

  rofi-workspace = rofi-workspace-cmd "rofi-workspace" (ws: "workspace ${ws}");
  rofi-move = rofi-workspace-cmd "rofi-move" (ws: "move window to workspace ${ws}");
in
{
  enable = true;
  font = "Fira Code 20";
  terminal = "${pkgs.rxvt-unicode}/bin/urxvt";
  extraConfig = ''
    rofi.modi: run#window#workspace:${rofi-workspace}#move:${rofi-move}
    rofi.kb-row-tab:
    rofi.kb-remove-to-eol:
    rofi.kb-accept-entry: Return
    rofi.kb-mode-next: Tab
    rofi.kb-mode-previous: Shift+Tab
    rofi.kb-row-up: Control+k
    rofi.kb-row-down: Control+j
  '';
}
