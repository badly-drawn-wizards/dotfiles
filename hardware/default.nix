{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./boot.nix
      ./filesystems.nix
      ./printing.nix
      ./bluetooth.nix
      ./joycon.nix
      ./avloopback.nix
      # ./iio.nix
    ];

  nix.settings.max-jobs = 8;
  #powerManagement.cpuFreqGovernor = "powersave";

  hardware = {
    enableAllFirmware = true;
    enableRedistributableFirmware = true;
    cpu.amd = {
      updateMicrocode = true;
    };
  };


  services.udev = {
    extraRules = ''
      KERNEL=="ttyUSB*", MODE="0777"
    '';
  };

}
