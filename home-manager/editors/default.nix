{ config, lib, pkgs, ... }:

{
  imports = [
    ./emacs
    ./vscode
    ./intellij
    ./vim.nix
  ];
}
