{ config, lib, pkgs, ... }:
let
  dotnet-sdk = pkgs.dotnet-sdk_10;
in
{
  home.packages = [
    dotnet-sdk
  ];

  home.sessionVariables = {
    "DOTNET_ROOT" = "${dotnet-sdk}/share/dotnet";
  };
}
