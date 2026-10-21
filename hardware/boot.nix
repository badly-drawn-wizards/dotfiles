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
        # Kernels + initrds live on the 260M ESP; ~50-60M per distinct kernel.
        configurationLimit = 10;
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

    # Wizard boot splash; also draws the LUKS passphrase prompt (systemd
    # initrd). No early amdgpu: its firmware would bloat the initrd on the
    # 260M ESP, and Plymouth draws on the firmware framebuffer without it.
    plymouth = {
      enable = true;
      theme = "dracula-wizard";
      themePackages = [ pkgs.dracula-wizard-plymouth ];
    };

    tmp.cleanOnBoot = true;

    supportedFilesystems = [ "btrfs" "ntfs" ];
  };
}
