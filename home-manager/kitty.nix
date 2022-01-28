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
      window_margin_width = 6;
    } // hashColors config.theme;
  };
}
