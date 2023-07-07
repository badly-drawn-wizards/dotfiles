{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [ 3000 ];
    };
  };
}
