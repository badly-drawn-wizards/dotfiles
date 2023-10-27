{ config, lib, pkgs, ... }:

{
  imports = [
    ./vagrant.nix
  ];

  virtualisation = {
    containers = {
      enable = true;
      containersConf.cniPlugins = with pkgs; [
        cni-plugins
        cni-plugin-flannel
      ];
    };

    docker = {
      enable = true;
    };
    podman = {
      enable = true;
    };

    libvirtd.enable = true;

    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };

  boot.kernelModules = [
      "kvm-intel"
  ];
}
