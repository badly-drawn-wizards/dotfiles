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
      kernelModules = [ "nfs" "amdgpu" ];
    };

    kernelParams = [
      "acpi_osi=Linux"
    ];

    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback.out
      config.boot.kernelPackages.acpi_call.out
    ];

    extraModprobeConfig = ''
    options asus_nb_wmi tablet_mode_sw=2
    '';

    kernelPackages = pkgs.linuxPackagesFor pkgs.linuxKernel.kernels.linux_custom;

    kernelModules = [
    ];

    tmp.cleanOnBoot = true;

    supportedFilesystems = [ "btrfs" "ntfs" "nfs" ];
  };
}
