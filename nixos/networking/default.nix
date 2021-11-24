{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;
    extraHosts =
      import ./spotify-sinkhole-hosts.nix;

    firewall = {
      enable = true;
      allowedTCPPorts = [ 3000 ];
    };
  };
}
