{ config, lib, pkgs, ... }:

{
  windowManager.startupPrograms = [
    "${pkgs.asusctl}/bin/rog-control-center"
  ];

  services = {
    clipman.enable = true;
    batsignal = {
      enable = true;
      extraArgs = [
        "-e"
        "-W"
        " Highway to the danger zone"
        "-C"
        " Ride into the danger zone"
        "-D"
        " Headin' into twilight"
      ];
    };
    # udiskie = {
    #   enable = true;
    #   tray = "always";
    # };
    pasystray.enable = true;
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
