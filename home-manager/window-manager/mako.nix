{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
in
{
  services.mako = {
    enable = true;
    layer = "overlay";
  };
  windowManager.extraBinds = {
    "${mod}+n" = "exec ${pkgs.mako}/bin/makoctl dismiss -a";
    "${mod}+Shift+n" = "exec ${pkgs.mako}/bin/makoctl dismiss";
  };
}
