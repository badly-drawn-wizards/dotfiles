{ config, lib, pkgs, ... }:
let
  dotnet-sdk = pkgs.dotnet-sdk_10;
in
{
  home.packages = [
    dotnet-sdk
  ];

  home.file.".dotnet-root".source = "${dotnet-sdk}/share/dotnet";

  home.sessionVariables = {
    "DOTNET_ROOT" = "${config.home.homeDirectory}/.dotnet-root";
  };
}
