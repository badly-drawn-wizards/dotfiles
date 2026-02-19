{ config, lib, pkgs, ... }:

{
  imports = [
    ./lean.nix
    ./python.nix
    ./java.nix
    ./scala.nix
    ./nix.nix
    ./latex.nix
    ./rust.nix
    ./dotnet.nix
  ];
}
