{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
  inherit (config) theme;
in
{
  services.swaync = {
    enable = true;
    settings = {
      positionX = "right";
      positionY = "top";

      widgets = [
        "inhibitors"
        "title"
        "dnd"
        "mpris"
        "notifications"
      ];
    };
  };

  windowManager.extraBinds = {
    "${mod}+n" = "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
    "${mod}+Shift+n" = "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
  };
}
