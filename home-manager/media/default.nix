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

    libva-utils
    glxinfo

    qpwgraph
  ];

  services = {
    easyeffects.enable = true;
    mpd.enable = true;
    mpd-mpris.enable = true;
    mpris-proxy.enable = true;
    playerctld.enable = true;
  };

}
