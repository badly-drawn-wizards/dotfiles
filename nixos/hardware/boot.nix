{ config, lib, pkgs, ... }:

{
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ "dm-raid" "dm-snapshot" ];
    };

    kernelPackages = pkgs.linuxPackages_latest;

    kernelModules = [ "kvm-intel" ];

    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };
}
