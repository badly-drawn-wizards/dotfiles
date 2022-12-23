{ config, lib, pkgs, ... }:

{
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
        MultiProfile = "multiple";
      };
      Policy = {
        AutoEnable = false;
      };
    };
  };

  services.blueman.enable = true;
}
