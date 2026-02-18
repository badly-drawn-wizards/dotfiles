{ config, lib, pkgs, ... }:

{
  services.gnome.gnome-keyring.enable = true;
  security = {
    polkit.enable = true;
    rtkit.enable = true;
    pam.services = {
      login = {
        enableGnomeKeyring = true;
        sshAgentAuth.enable = true;
        gnupg.enable = true;
      };
      swaylock.enableGnomeKeyring = true;
    };
  };
}
