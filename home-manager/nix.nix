{ config, osConfig, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
      nix-prefetch
      cachix
      fup-repl
      nixpkgs-fmt
  ];

  programs = {
    nix-index.enable = true;
    nix-index-database = {
      comma.enable = true;
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
