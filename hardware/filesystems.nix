{ config, lib, pkgs, ... }:

{
  boot.initrd.luks.devices = {
    root = {
      # By PARTUUID: Windows renumbers partitions.
      device = "/dev/disk/by-partuuid/fc10d25a-fd48-480d-969d-148df73c61ba";
      preLVM = true;
    };
  };
  fileSystems = {
    "/boot" = {
      # ESP after the LUKS partition, with free space after it to grow into.
      device = "/dev/disk/by-partuuid/4e4504c7-e646-4ed9-b150-ba78d57f2b6e";
      fsType = "vfat";
      # Root-only: holds bootctl's random seed and the Secure Boot files.
      options = [ "fmask=0077" "dmask=0077" ];
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
