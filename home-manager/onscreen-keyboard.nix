{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.modifier;
in
{
  # TODO: the rest of the owl
  windowManager.startupPrograms = [
    { command = "${pkgs.fcitx5}/bin/fcitx"; always = true; }
    "${pkgs.squeekboard}/bin/squeekboard"
  ];
}
