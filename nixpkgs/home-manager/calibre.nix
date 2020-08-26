{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    calibre
  ];
  home.sessionVariables.CALIBRE_USE_DARK_PALETTE = "1";
}
