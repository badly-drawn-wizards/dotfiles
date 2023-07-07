{ config, lib, pkgs, ... }:

{
  imports = [
    ./tree-sitter.nix
    ./lean.nix
    ./python.nix
    ./java.nix
  ];
}
