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
      # Probe each connection for internet access. One that fails gets its
      # default route penalised, so e.g. a phone hotspot wins over Ethernet
      # to a router whose uplink is down.
      settings.connectivity = {
        uri = "http://nmcheck.gnome.org/check_network_status.txt";
        response = "NetworkManager is online";
        interval = 30;
      };
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

  # Tailnet client. "client" lets it use subnet routes if a node advertises
  # any. One-time: sudo tailscale up --accept-routes
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };

  # Tailscale's split DNS (*.internal -> millionants) needs resolved;
  # NetworkManager hands its DNS to resolved automatically when enabled.
  services.resolved.enable = true;

  systemd.services.NetworkManager-wait-online.enable = false;
}
