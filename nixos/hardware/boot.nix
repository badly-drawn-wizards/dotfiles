{ config, lib, pkgs, ... }:

{
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ "wl" "dm-raid" "dm-snapshot" ];
    };

    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
    kernelModules = [ "wl" "kvm-intel" ];

    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };
}
