{ config, lib, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [
    6443 # k3s: required so that pods can reach the API server (running on port 6443 by default)
    # 2379 # k3s, etcd clients: required if using a "High Availability Embedded etcd" configuration
    # 2380 # k3s, etcd peers: required if using a "High Availability Embedded etcd" configuration
  ];
  services.k3s.enable = true;

  services.k3s.role = "server";
  services.k3s.extraFlags = toString [
    "--write-kubeconfig-mode 0644"
    "--container-runtime-endpoint unix:///run/containerd/containerd.sock"
  ];

}
