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
        gfxmodeEfi = "1920x1440,3840x2160,auto";
        device = "nodev";
      };
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ "i915" ];
    };

    kernelParams = [
      "intel_iommu=on"
    ];

    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback.out
    ];
    kernelPackages = pkgs.linuxPackages_latest;

    kernelModules = [
      # For screen-sharing
      "v4l2loopback"
      "snd-aloop"

      # Virtualization
      "kvm-intel"
    ];

    extraModprobeConfig = ''
    options v4l2loopback exclusive_caps=1 card_label="Screen"
    '';

    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };
}
