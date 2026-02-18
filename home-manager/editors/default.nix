{ config, lib, pkgs, ... }:

{
  imports = [
    ./vscode
    ./intellij
    ./vim
  ];

  home.packages = with pkgs; [
    ripgrep
    postman
  ];
}
