{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
  swaylock = config.programs.swaylock.package;
  swaylock-cmd = "${swaylock}/bin/swaylock -f";
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
    events = [
      { event = "before-sleep"; command = swaylock-cmd; }
      { event = "lock"; command = swaylock-cmd; }
    ];
  };

  windowManager.extraBinds = {
    "${mod}+Shift+a" = "exec ${swaylock}/bin/swaylock";
  };

}
