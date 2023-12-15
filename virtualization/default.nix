{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ./vagrant.nix
    # ./k3s.nix
    # ./kata.nix
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

    podman.enable = true;
    docker.enable = true;

    cri-o.enable = true;

    libvirtd = {
      enable = true;
      extraConfig = ''
        unix_sock_group = "libvirtd"
        unix_sock_rw_perms = "0770"
      '';
      qemu.ovmf = {
        enable = true;
        packages = [ pkgs.OVMFFull.fd ];
      };
      qemu.swtpm.enable = true;
    };

  };

  services.dockerRegistry = {
    enable = true;
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

  # boot.kernelModules = [
  #   "kvm-amd"
  # ];

  environment.systemPackages = with pkgs; [
    kubectl
    containerd
    kata-runtime
  ];

}
