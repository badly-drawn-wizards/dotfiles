{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;
    extraHosts = lib.concatLines [
      (import ./spotify-sinkhole-hosts.nix)
      (import ./whirlylabs-hosts.nix)
    ];

    firewall = {
      enable = true;
      allowedTCPPorts = [ 3000 ];
    };
  };
}
