{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    # dotnet-sdk_11
    dotnet-sdk_10
  ];
}
