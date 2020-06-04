{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
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
