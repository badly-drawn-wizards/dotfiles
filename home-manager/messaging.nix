{ config, lib, pkgs, ... }:

{

  windowManager.startupPrograms = with pkgs; [
    "${thunderbird}/bin/thunderbird"
    "${zulip}/bin/zulip"
    "${discord}/bin/discord"
    "${element-desktop}/bin/element-desktop --profile personal"
    "${mattermost-desktop}/bin/mattermost-desktop"
  ];
  home.packages = with pkgs; [
    # teams
    # zoom-us
    discord
    element-desktop
    slack
    zulip
    mattermost-desktop
    #franz
    #skypeforlinux
    #whatsapp-for-linux
  ];
}
