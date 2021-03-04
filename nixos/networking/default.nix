{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;
    extraHosts =
      import ./spotify-sinkhole-hosts.nix;

    firewall = {
      enable = true;
      allowedTCPPorts = [ 8080 8303 ];
      allowedUDPPorts = [ 8303 ];
    };
  };
}
