{ config, lib, pkgs, ... }:

{
  imports = [
    ./games.nix
    ./calibre.nix
    ./streaming.nix
  ];

  home.packages = with pkgs; [
    vlc
    spotify

    libva-utils

    qpwgraph
  ];

  programs.mpv.enable = true;

  services = {
    easyeffects.enable = true;
    mpris-proxy.enable = true;
    playerctld.enable = true;
  };

}
