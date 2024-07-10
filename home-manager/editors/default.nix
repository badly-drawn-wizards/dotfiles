{ config, lib, pkgs, ... }:

{
  imports = [
    ./emacs
    ./vscode
    ./intellij
    ./vim
  ];

  home.packages = with pkgs; [
    ripgrep
    postman
  ];
}
