{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ keepassxc ];

  windowManager.startupPrograms = with pkgs; [
    "${polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
    "${keepassxc}/bin/keepassxc"
  ];
}
