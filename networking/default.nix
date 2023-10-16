{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;

    firewall = {
      enable = true;
      checkReversePath = false;
    };
  };
}
