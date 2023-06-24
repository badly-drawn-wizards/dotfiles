{ config, lib, pkgs, ... }:

{

  windowManager.startupPrograms = with pkgs; [
    "${firefox}/bin/firefox"
  ];

  import = [
    ./firefox.nix
    ./nyxt
  ];
}
