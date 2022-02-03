{ config, lib, pkgs, ... }:

{
  fileSystems = {
    "/" = {
      device = "/dev/vg0/nixos";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/nvme0n1p1";
      fsType = "vfat";
    };
  };
  swapDevices = [ { device = "/dev/vg0/swap"; } ];
}
