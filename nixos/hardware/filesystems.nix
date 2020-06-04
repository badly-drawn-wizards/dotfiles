{ config, lib, pkgs, ... }:

{
  fileSystems = {
    "/" = {
      device = "/dev/vg/root";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/nvme0n1p1";
      fsType = "vfat";
    };
  };
  swapDevices = [ ];

}
