{ config, lib, pkgs, ... }:

{
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-partuuid/fc10d25a-fd48-480d-969d-148df73c61ba"; # nvme0n1p5, "nixos"
      preLVM = true;
    };
  };
  fileSystems = {
    "/boot" = {
      # nvme0n1p8, 4G ESP in the gap after the LUKS partition (room to grow
      # into the free space after it). The original 260M ESP is nvme0n1p1,
      # PARTUUID be7c42a1-9f73-4e94-92b5-542549624b93.
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
