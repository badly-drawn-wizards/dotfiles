{ config, lib, pkgs, ... }:

{
  services = {
    udev.packages = [ pkgs.joycond ];
  };
  systemd.packages = [ pkgs.joycond ];
  environment.systemPackages = [ pkgs.joycond ];

  boot.kernelModules = [
    "hid-nintendo"
  ];
}
