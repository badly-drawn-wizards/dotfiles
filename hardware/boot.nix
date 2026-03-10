{ config, lib, pkgs, ... }:

{
  boot = {
    loader = {
      # systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        enableCryptodisk = true;
        theme = pkgs.dracula-grub-theme;
        device = "nodev";
        useOSProber = true;
      };
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [
      ];
    };

    kernelParams = [
    ];

    extraModulePackages = [
    ];

    # kernelPackages = pkgs.linuxPackages_6_5;
    kernelPackages = pkgs.linuxPackages_latest;

    kernelModules = [
    ];

    tmp.cleanOnBoot = true;

    supportedFilesystems = [ "btrfs" "ntfs" ];
  };
}
