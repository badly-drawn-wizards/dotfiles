{ config, lib, pkgs, ... }:

{
  imports = [
    ./tree-sitter.nix
    ./lean.nix
    ./python.nix
    ./java.nix
    ./scala.nix
    ./nix.nix
    ./latex.nix
    ./rust.nix
    # ./sage.nix
  ];
}
