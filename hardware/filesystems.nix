{ config, lib, pkgs, ... }:

{
  boot.initrd.luks.devices = {
    root = {
      # Partition labelled "nixos". Always by PARTUUID: Windows renumbers
      # partitions (it went p7 -> p5 -> p4 so far).
      device = "/dev/disk/by-partuuid/fc10d25a-fd48-480d-969d-148df73c61ba";
      preLVM = true;
    };
  };
  fileSystems = {
    "/boot" = {
      # 4G ESP right after the LUKS partition, with ~42G free after it to grow
      # into. Replaced the original 260M ESP at the start of the disk, which
      # Windows deleted (diskpart) after bcdboot moved its files here.
      device = "/dev/disk/by-partuuid/4e4504c7-e646-4ed9-b150-ba78d57f2b6e";
      fsType = "vfat";
      # Root-only: bootctl's random seed (and later Secure Boot files) live here.
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
