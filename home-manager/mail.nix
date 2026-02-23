{ config, lib, pkgs, ... }:

{
  programs.thunderbird = {
    enable = true;
    profiles."5zqvrdin.default".isDefault = true;
  };
}
