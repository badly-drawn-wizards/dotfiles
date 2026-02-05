{ config, lib, pkgs, ... }:

{
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p7";
      preLVM = true;
    };
  };
  fileSystems = {
    "/boot" = {
      device = "/dev/nvme0n1p1";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/vg/root";
      fsType = "btrfs";
      options = [ "subvol=root" "compress=zstd" "noatime" ];
    };
    "/nix" = {
      device = "/dev/vg/root";
      fsType = "btrfs";
      options = [ "subvol=nix" "compress=zstd" "noatime" ];
      
    };
    "/home" = {
      device = "/dev/vg/root";
      fsType = "btrfs";
      options = [ "subvol=home" "compress=zstd" "noatime" ];
    };
  };
  swapDevices = [ { device = "/dev/vg/swap"; } ];
  }
