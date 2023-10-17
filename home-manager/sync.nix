{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ maestral-gui ];
}
