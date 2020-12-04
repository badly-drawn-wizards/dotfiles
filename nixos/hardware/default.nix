{ config, lib, pkgs, ... }:

{
  imports =
    [ pkgs.nixosModules.notDetected
      ./filesystems.nix
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
