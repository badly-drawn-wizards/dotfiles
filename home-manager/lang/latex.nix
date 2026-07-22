{ config, lib, pkgs, ... }:

{
  home.packages = [
    (pkgs.texliveSmall.withPackages (tpkgs: with tpkgs; [
      scheme-medium
      ticket
      datatool
      graphics
      graphicxbox
      minted
    ]))
  ];
}
