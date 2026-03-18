{ config, lib, pkgs, ... }:

{
  programs.thunderbird = {
    enable = false;
    profiles."5zqvrdin.default".isDefault = true;
  };

  windowManager.startupPrograms = [
    # "${thunderbird}/bin/thunderbird"

    # Set in NixOS configuration for now
    "evolution"
  ];

}
