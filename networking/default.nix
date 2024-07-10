{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager = {
      enable = true;
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

  systemd.services.NetworkManager-wait-online.enable = false;
}
