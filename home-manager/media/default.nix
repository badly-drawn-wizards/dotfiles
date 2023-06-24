{ config, lib, pkgs, ... }:

{
  imports = [
    ./games.nix
    ./calibre.nix
    ./streaming.nix
  ];
  home.packages = with pkgs; [
      mpv
      spotify

      libva-utils glxinfo
  ];
}
