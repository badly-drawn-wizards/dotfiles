{ config, lib, pkgs, ... }:

let
  rnix-lsp-alias = pkgs.writeScriptBin "rnix-lsp" ''
    #!${pkgs.bash}/bin/bash
    exec ${pkgs.nixd}/bin/nixd
  '';
in
{
  home.packages = with pkgs; [
    nixd
    rnix-lsp-alias
  ];
}
