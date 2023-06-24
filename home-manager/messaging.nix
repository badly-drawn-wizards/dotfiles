{ config, lib, pkgs, ... }:

{

  windowManager.startupPrograms = with pkgs; [
    "${discord}/bin/discord"
  ];
  home.packages = with pkgs; [
      teams
      zoom-us
      discord
      element-desktop
      slack
      zulip
      franz
      skypeforlinux
  ];
}
