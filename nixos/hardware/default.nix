{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./filesystems.nix
      ./boot.nix
    ];

  nix.maxJobs = 8;
  powerManagement.cpuFreqGovernor = "powersave";

  hardware = {
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;

    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
    };

    sensor = {
      iio.enable = true;
    };

  };

}
