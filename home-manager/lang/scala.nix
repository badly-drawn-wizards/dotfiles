{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ sbt coursier ];
}
