{ config, osConfig, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
      nix-prefetch
      cachix
      fup-repl
  ];

  home.sessionVariables.DOTFILES = "/workspace/dotfiles";

}
