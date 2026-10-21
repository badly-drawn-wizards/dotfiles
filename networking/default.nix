{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager = {
      enable = true;
      plugins = with pkgs; [
        networkmanager-openvpn
        networkmanager-openconnect
        networkmanager-strongswan
      ];
    };

    firewall = {
      enable = true;
      allowedTCPPorts = [
        # 8007
        # 8080
        # 8081
      ];
    };

  };

  services.strongswan.enable = true;

  # Tailnet client. "client" lets it use subnet routes (millionants
  # advertises 192.168.0.109/32). One-time: sudo tailscale up --accept-routes
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };

  # Tailscale's split DNS (*.internal -> millionants) needs resolved;
  # NetworkManager hands its DNS to resolved automatically when enabled.
  services.resolved.enable = true;

  systemd.services.NetworkManager-wait-online.enable = false;
}
