{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ./vagrant.nix
  ];

  virtualisation = {
    # containers = {
    #   enable = true;
    #   containersConf.cniPlugins = with pkgs; [
    #     cni-plugins
    #     cni-plugin-flannel
    #   ];
    # };

    containerd.enable = true;

    # podman.enable = true;
    docker.enable = true;

    # cri-o.enable = true;

    libvirtd = {
      enable = true;
      extraConfig = ''
        unix_sock_group = "libvirtd"
        unix_sock_rw_perms = "0770"
      '';
      qemu.swtpm.enable = true;
    };

  };

  # boot.kernelModules = [
  #   "kvm-amd"
  # ];

  environment.systemPackages = with pkgs; [
    kubectl
    containerd
  ];

}
