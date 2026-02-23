{ config, lib, pkgs, ... }:

{
  imports = [
    ./vscode
    ./intellij
    ./vim
  ];

  home.packages = with pkgs; [
    postman
  ];

  programs.ripgrep.enable = true;
}
