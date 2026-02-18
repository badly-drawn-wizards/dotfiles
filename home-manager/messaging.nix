{ config, lib, pkgs, ... }:

{

  windowManager.startupPrograms = with pkgs; [
    "${thunderbird}/bin/thunderbird"
    "${zulip}/bin/zulip"
    "${discord}/bin/discord"
    ''${element-desktop}/bin/element-desktop --profile personal --password-store="gnome-libsecret"''
    "${teams-for-linux}/bin/teams-for-linux"
  ];
  home.packages = with pkgs; [
    # zoom-us
    teams-for-linux
    discord
    element-desktop
    zulip
    #whatsapp-for-linux
  ];
}
