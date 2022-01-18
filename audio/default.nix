{ config, lib, pkgs, ... }:

{
  sound = {
    enable = true;
  };

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    support32Bit = true;
    # configFile = pkgs.writeText "default.pa" ''
    #   load-module module-bluetooth-policy
    #   load-module module-bluetooth-discover
    #   ## module fails to load with
    #   ##   module-bluez5-device.c: Failed to get device path from module arguments
    #   ##   module.c: Failed to load module "module-bluez5-device" (argument: ""): initialization failed.
    #   # load-module module-bluez5-device
    #   # load-module module-bluez5-discover
    # '';
  };

  hardware.bluetooth = {
      settings = {
        General = {
          # Enable = "Source,Sink,Media,Socket";
        };
      };
  };

  services.pipewire.enable = true;
}
