{ config, lib, pkgs, ... }:

{
  environment.variables = {
    WLR_NO_HARDWARE_CURSORS="1";
  };
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
    ];
  };

  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    open = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.latest;
  };
}
