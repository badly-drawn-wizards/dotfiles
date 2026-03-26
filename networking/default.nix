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

  systemd.services.NetworkManager-wait-online.enable = false;
}
