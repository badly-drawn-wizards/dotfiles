{ config, lib, pkgs, ... }:

{
  imports = [
    ./firefox.nix
    ./nyxt
  ];

  windowManager.startupPrograms = with pkgs; [
    "${firefox}/bin/firefox"
  ];

}
