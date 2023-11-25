{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ./vagrant.nix
    ./k3s.nix
    ./kata.nix
  ];

  virtualisation = {
    containers = {
      enable = true;
      containersConf.cniPlugins = with pkgs; [
        cni-plugins
        cni-plugin-flannel
      ];
    };

    containerd.enable = true;

    podman = {
      enable = true;
      dockerSocket.enable = true;
    };

    cri-o.enable = true;

    libvirtd.enable = true;

    virtualbox.host.enable = true;
  };

  # microvm = {
  #   host.enable = true;
  #   vms = {
  #     k8s-vm = {
  #       flake = inputs.k8s-vm;
  #     };
  #   };
  #   autostart = [ ];
  # };

  boot.kernelModules = [
    "kvm-amd"
  ];

  environment.systemPackages = with pkgs; [
    containerd
    kata-runtime
  ];

}
