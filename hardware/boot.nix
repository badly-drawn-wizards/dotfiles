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
      "i8042.unlock=1"
    ];

    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback.out
      config.boot.kernelPackages.acpi_call.out
    ];

    kernelPackages = pkgs.linuxPackagesFor pkgs.linuxKernel.kernels.linux_custom;

    kernelModules = [
      "acpi_call"
    ];

    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };
}
