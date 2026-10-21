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
      device = "/dev/disk/by-partuuid/be7c42a1-9f73-4e94-92b5-542549624b93"; # nvme0n1p1, EFI system partition
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
