{ config, lib, pkgs, ... }:

{

  windowManager.startupPrograms = with pkgs; [
    "${discord}/bin/discord"
  ];
  home.packages = with pkgs; [
      # teams
      # zoom-us
      discord
      (element-desktop.override { electron = pkgs.electron_24; })
      slack
      zulip
      franz
      skypeforlinux
      whatsapp-for-linux
  ];
}
