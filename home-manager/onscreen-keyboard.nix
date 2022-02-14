{ config, lib, pkgs, ... }:

let
  mod = config.windowManager.sway;
in
{
  # TODO: the rest of the owl
  windowManager.startupPrograms = [
    { command = "${pkgs.fcitx5}"; always = true; }
    "${pkgs.squeekboard}/bin/squeekboard"
  ];
}
