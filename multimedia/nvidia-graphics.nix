{ config, lib, pkgs, ... }:

{
  environment.variables = {
    WLR_NO_HARDWARE_CURSORS = "1";
  };
  hardware.graphics = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    open = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.latest;
  };
}
