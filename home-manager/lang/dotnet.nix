{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    dotnet-sdk_11
  ];
}
