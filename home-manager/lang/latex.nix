{ config, lib, pkgs, ... }:

{

  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: { 
      inherit (tpkgs) 
      scheme-medium
      ticket
      datatool
      graphics
      graphicxbox;
    };
  };
}
