{ config, lib, pkgs, ... }:

let
  hashColors = builtins.mapAttrs (k: v: "#${v}");
in
{
  programs.kitty = {
    enable = true;
    settings = {
      enable_audio_bell = false;
      font_family = config.font;
      font_size = config.fontSize;
      background_opacity = "0.98";
    } // hashColors config.theme;
  };
}
