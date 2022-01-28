{ config, lib, pkgs, ... }:

{
  boot = {
    loader = {
      # systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        theme = pkgs.dracula-grub-theme;
        gfxmodeEfi = "1920x1440,auto";
        device = "nodev";
      };
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ "i915" "dm-raid" "dm-snapshot" ];
    };

    kernelParams = [
      "intel_iommu=on"
    ];

    kernelPackages = pkgs.linuxPackages_latest;

    kernelModules = [
      "kvm-intel"
    ];

    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };
}
