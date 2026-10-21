{ config, lib, pkgs, ... }:

{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      # Lanzaboote replaces the systemd-boot module: it installs systemd-boot
      # and signs a stub per generation with the sbctl keys below.
      systemd-boot.enable = lib.mkForce false;
    };

    # Secure Boot. Keys: sudo sbctl create-keys (in /var/lib/sbctl, root
    # only); enroll with sbctl enroll-keys --microsoft --firmware-builtin
    # once the firmware is in setup mode. Windows Boot Manager on the ESP
    # still gets a menu entry automatically.
    lanzaboote = {
      enable = true;
      pkiBundle = "/var/lib/sbctl";
      configurationLimit = 10; # the 4G ESP fits far more; keeps the menu short
      # No kernel cmdline editing at boot (init=/bin/sh). Defaults to
      # systemd-boot.editor, which is true.
      settings.editor = false;
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
    # initrd). Plymouth uses the firmware framebuffer, so no early amdgpu
    # (its firmware would bloat the initrd).
    plymouth = {
      enable = true;
      theme = "dracula-wizard";
      themePackages = [ pkgs.dracula-wizard-plymouth ];
    };

    tmp.cleanOnBoot = true;

    supportedFilesystems = [ "btrfs" "ntfs" ];
  };

  # Secure Boot key management and verification (sbctl status / verify).
  environment.systemPackages = [ pkgs.sbctl ];
}
