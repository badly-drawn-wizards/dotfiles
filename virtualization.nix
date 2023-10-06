{ config, lib, pkgs, ... }:

{
  virtualisation = {
    containers = {
      enable = true;
      containersConf.cniPlugins = with pkgs; [
        cni-plugins
        cni-plugin-flannel
      ];
    };

    podman = {
      enable = true;
      dockerCompat = true;
    };

    libvirtd.enable = true;
  };

  boot.kernelModules = [
      "kvm-intel"
  ];
}
