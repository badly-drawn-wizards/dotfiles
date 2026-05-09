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

    extraHosts = ''
      10.240.0.129 masscloud-recon-staging.database.windows.net
      10.250.5.5 masscloud-prod.database.windows.net
    '';

  };

  services.strongswan.enable = true;

  systemd.services.NetworkManager-wait-online.enable = false;
}
