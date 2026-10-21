{ config, lib, pkgs, ... }:

{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      # systemd-boot (Lanzaboote needs it for Secure Boot). Windows Boot
      # Manager on the same ESP gets a menu entry automatically, so no
      # os-prober equivalent is needed.
      systemd-boot = {
        enable = true;
        # Each distinct kernel+initrd is ~51M; the 4G ESP fits far more than
        # 10, this just keeps the menu short.
        configurationLimit = 10;
        editor = false; # no kernel cmdline editing at boot (init=/bin/sh)
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
