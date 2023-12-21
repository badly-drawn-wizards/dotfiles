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
        # gfxmodeEfi = "1920x1440,3840x2160,auto";
        device = "nodev";
      };
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [
        "nfs"
        # "radeon"
      ];
    };

    kernelParams = [
      "acpi_osi=Linux"
    ];

    extraModulePackages = [
      # config.boot.kernelPackages.v4l2loopback.out
      config.boot.kernelPackages.acpi_call.out
    ];

    kernelPackages = pkgs.linuxPackages_6_5;
    # kernelPackages = pkgs.linuxPackages_6_6;

    kernelModules = [
    ];

    tmp.cleanOnBoot = true;

    supportedFilesystems = [ "btrfs" "ntfs" "nfs" ];
  };
}
