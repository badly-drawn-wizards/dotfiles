{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager = {
      enable = true;
    };

    firewall = {
      enable = true;
      checkReversePath = false;
      # allowedTCPPorts = [ 6443 ];
    };

  };

  systemd.services.NetworkManager-wait-online.enable = false;
}
