{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
      thunderbird
      betterbird
  ];
}
