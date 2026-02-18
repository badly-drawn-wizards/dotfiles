{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
  swaylock = config.programs.swaylock.package;
  swaylock-cmd = toString (pkgs.writeScript "swaylock-cmd" ''
  #!${pkgs.bash}/bin/bash
  ${pkgs.sway}/bin/swaymsg input type:keyboard events enabled
  ${pkgs.sway}/bin/swaymsg input type:touchpad events enabled
  ${pkgs.sway}/bin/swaymsg output eDP-1 transform normal
  exec ${swaylock}/bin/swaylock -f
  '');
in
{
  programs.swaylock = {
    enable = true;
    settings = {
      image = config.backgrounds.current;
      color = "#${config.theme.color0}";
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 300; command = swaylock-cmd; }
    ];
    events = {
      before-sleep = swaylock-cmd;
      lock = swaylock-cmd;
    };
  };

  windowManager.extraBinds = {
    "${mod}+Shift+a" = "exec ${swaylock}/bin/swaylock";
  };

}
