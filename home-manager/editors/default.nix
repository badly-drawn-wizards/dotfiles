{ config, lib, pkgs, ... }:

{
  imports = [
    # ./emacs
    ./vscode
    ./intellij
    ./vim
  ];

  home.packages = with pkgs; [
    tree-sitter
    espeak
    ripgrep
    libnotify
  ];
}
