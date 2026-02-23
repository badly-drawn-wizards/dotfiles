{ config, lib, pkgs, ... }:

{
  programs.calibre.enable = true;

  home.sessionVariables.CALIBRE_USE_DARK_PALETTE = "1";
}
