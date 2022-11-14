{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./boot.nix
      ./filesystems.nix
      ./printing.nix
    ];

  nix.settings.max-jobs = 8;
  powerManagement.cpuFreqGovernor = "powersave";

  hardware = {
    enableAllFirmware = true;
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;

    bluetooth = {
      enable = true;
      package = pkgs.bluez;
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

    sensor = {
      iio.enable = true;
    };

  };

  services.udev = {
    extraRules = ''
      KERNEL=="ttyUSB*", MODE="0777"
    '';
  };


}