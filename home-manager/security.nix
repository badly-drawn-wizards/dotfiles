{ config, lib, pkgs, ... }:

{
  windowManager.startupPrograms = with pkgs; [
    # "${polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
  ];

  programs.zsh.initExtra = ''
    eval $(gnome-keyring-daemon --start --daemonize 2>&-)
    export SSH_AUTH_SOCK
  '';
}
