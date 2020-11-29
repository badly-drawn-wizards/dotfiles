{ config, lib, pkgs, ... }:

{
  imports =
    [ ./filesystems.nix
      ./boot.nix
    ];

  nix.maxJobs = 8;
  powerManagement.cpuFreqGovernor = "powersave";

  hardware = {
    cpu.intel.updateMicrocode = true;

    bluetooth = {
      enable = true;
    };

    sensor = {
      iio.enable = true;
    };

  };

}
