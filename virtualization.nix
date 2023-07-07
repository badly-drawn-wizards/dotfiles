{ config, lib, pkgs, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };

    libvirtd.enable = true;

    oci-containers = {
      backend = "docker";
    };
  };

  boot.kernelModules = [
      "kvm-intel"
  ];
}
