{ config, lib, pkgs, ... }:

{

  windowManager.startupPrograms = with pkgs; [
    "${thunderbird}/bin/thunderbird"
    "${zulip}/bin/zulip"
    "${discord}/bin/discord"
    "${element-desktop}/bin/element-desktop"
    "${element-desktop}/bin/element-desktop --profile personal"
  ];
  home.packages = with pkgs; [
      # teams
      # zoom-us
      discord
      element-desktop
      slack
      zulip
      #franz
      #skypeforlinux
      #whatsapp-for-linux
  ];
}
