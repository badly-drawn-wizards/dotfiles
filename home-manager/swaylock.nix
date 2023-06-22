{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
  swaylock = config.programs.swaylock.package;
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
    events = [
      { event = "before-sleep"; command = "${swaylock}/bin/swaylock"; }
      { event = "lock"; command = "${swaylock}/bin/swaylock"; }
    ];
  };

  windowManager.extraBinds = {
    "${mod}+Shift+a" = "exec ${swaylock}/bin/swaylock";
  };

}
