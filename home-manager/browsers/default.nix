{ config, lib, pkgs, ... }:

{
  imports = [
    ./firefox.nix
    ./chromium.nix
    ./nyxt
  ];

  windowManager.startupPrograms = with pkgs; [
    "${firefox}/bin/firefox"
  ];

}
