{ config, lib, pkgs, ... }:

let
  obs-wlrobs = "${pkgs.obs-wlrobs}/share/obs/obs-plugins/wlrobs";
in
{
  # home.file = {
  #   ".config/obs-studio/plugins/wlrobs".source = obs-wlrobs;
  # };
}
