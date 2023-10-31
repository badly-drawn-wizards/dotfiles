{ config, lib, pkgs, inputs, ... }:

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

  microvm = {
    host.enable = true;
    vms = {
      k8s-vm = {
        flake = inputs.k8s-vm;
      };
    };
    autostart = [ ];
  };

  systemd.services."microvm-tap-interfaces@".serviceConfig = {
    AmbientCapabilities = [ "CAP_NET_ADMIN" ];
  };

  boot.kernelModules = [
    "kvm-intel"
  ];
}
