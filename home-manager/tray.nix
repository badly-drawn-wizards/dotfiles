{ config, lib, pkgs, ... }:

{
  services = {
    clipman.enable = true;
    batsignal = {
      enable = true;
      extraArgs = [
        "-e"
        "-W" " Highway to the danger zone"
        "-C" " Ride into the danger zone"
        "-D" " Headin' into twilight"
      ];
    };
    udiskie = {
      enable = true;
      tray = "always";
    };
    pasystray.enable = true;
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
