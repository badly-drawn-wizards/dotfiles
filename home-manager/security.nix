{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ keepassxc ];

  windowManager.startupPrograms = with pkgs; [
    "${polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
  ];

  programs.zsh.initExtraDag = {
    gnome-keyring = lib.hm.dag.entryAnywhere ''
      eval $(gnome-keyring-daemon --start --daemonize 2>&-)
      export SSH_AUTH_SOCK
    '';
  };
}
