{ config, lib, pkgs, ... }:

let
  eventScript = pkgs.writeScript "pomoOnEvent.sh" ''
    #!${pkgs.bash}/bin/bash
    if [ "$POMO_STATE" == "COMPLETE" ]
    then
      ${pkgs.libnotify}/bin/notify-send -u critical -i "${./icon.png}" "Pomodoro over"
    fi
  '';
in
{
  home.file = {
    ".config/pomo/config.json".text = ''
    {
      "onEvent": ["${pkgs.bash}/bin/bash", "${eventScript}"]
    }
    '';
  };

  home.packages = [
    pkgs.pomo
  ];
}
