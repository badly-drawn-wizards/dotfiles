{ config, lib, pkgs, ... }:

let
  hashColors = builtins.mapAttrs (k: v: "#${v}");
in
{
  programs.kitty = {
    enable = true;

    shellIntegration.mode = "enabled";
    shellIntegration.enableZshIntegration = true;

    settings = {
      enable_audio_bell = false;
      font_family = config.font;
      font_size = config.fontSize;
      background_opacity = "0.95";
      window_margin_width = 6;
      # # Temporary fix for scaling bug in libwayland-cursor
      # linux_display_server = "x11";
      confirm_os_window_close = -1;
    } // hashColors config.theme;
  };
}
