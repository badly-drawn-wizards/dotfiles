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

  # Tailnet client. "client" lets it use subnet routes if a node advertises any.
  # The prefs live in /var/lib/tailscale/tailscaled.state, and RouteAll=true is
  # persisted there, so `--accept-routes` does not need repeating.
  # There is no tailscaled-autoconnect unit in this nixpkgs and none is needed:
  # tailscaled reconnects by itself at boot. Verified 2026-11-18 — it registered
  # with the control server and had 100.76.41.56 one second after eth0 got an
  # address, with no manual `tailscale up`. If the tailnet is unreachable at boot
  # the fault is below it (link/DHCP), so diagnose that instead of adding a unit.
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };

  # Tailscale's split DNS (*.internal -> millionants) needs resolved;
  # NetworkManager hands its DNS to resolved automatically when enabled.
  services.resolved.enable = true;

  systemd.services.NetworkManager-wait-online.enable = false;
}
