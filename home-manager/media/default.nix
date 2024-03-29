{ config, lib, pkgs, ... }:

{
  imports = [
    ./games.nix
    ./calibre.nix
    ./streaming.nix
  ];

  home.packages = with pkgs; [
      mpv
      vlc
      spotify

      libva-utils glxinfo

      qpwgraph
  ];

  services = {
    easyeffects.enable = true;
    mpd.enable = true;
    mpd-mpris.enable = true;
    mpris-proxy.enable = true;
    playerctld.enable = true;
    spotifyd = {
      enable = true;
      package = pkgs.spotifyd.override { withKeyring = true; withPulseAudio = true; withMpris = true; };
      settings = {
        global = {
          username = "akbj8i88tfzgtbpvuus60fgb7";
          device_name = "noobnoob";
          device = "default";
          control = "default";
          use_keyring = true;
          use_mpris = true;
          backend = "alsa";
          autoplay = true;
        };
      };
    };
  };

  home.file = {
    ".local/share/soundfonts/fluid.sf2".source = "${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM2-2.sf2";
  };

}
