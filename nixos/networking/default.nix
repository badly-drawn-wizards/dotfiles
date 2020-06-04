{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;
    extraHosts =
      import ./spotify-sinkhole-hosts.nix;
  };
}
