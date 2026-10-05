{ config, lib, pkgs, ... }:

{
  hardware.bluetooth = {
    enable = true;
  };

  hardware.xpadneo.enable = true;

  services.blueman.enable = true;
}
