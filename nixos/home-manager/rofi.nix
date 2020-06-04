{ pkgs
, lib
, ...
}:

let
  # Adapted from
  # https://blog.sarine.nl/2014/08/03/rofi-updates.html
  rofi-workspaces = pkgs.writeScriptBin
    "rofi-workspaces.sh"
    ''
    #!/bin/sh
    if test -z $@
    then
      i3-msg -t get_workspaces | ${pkgs.jq}/bin/jq -r ".[].name"
    else
      i3-msg workspace $@ >/dev/null
    fi
    '';
in
{
  enable = true;
  font = "Fira Code 20";
  terminal = "${pkgs.rxvt-unicode}/bin/urxvt";
  extraConfig = ''
    rofi.modi: window#workspace:${rofi-workspaces}/bin/rofi-workspaces.sh#run
  '';
}
