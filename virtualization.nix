{ config, lib, pkgs, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };

    libvirtd.enable = true;
  };

  boot.kernelModules = [
      "kvm-intel"
  ];
}
