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

    # TODO Learn what this does
    kernelParams = [ "intel_iommu=on" ];

    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
    kernelModules = [ "wl" "kvm-intel" ];

    # TODO Determine whether this actually does anything
    # extraModprobeConfig = ''
    #   option snd_hda_intel enable=true model=laptop-amic
    # '';

    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };
}
