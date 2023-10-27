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
    };
    nameservers = [ "1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
  };

  services.resolved = {
    enable = true;
    dnssec = "true";
  };
}
