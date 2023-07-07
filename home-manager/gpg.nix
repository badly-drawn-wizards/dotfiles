{ config, lib, pkgs, ... }:

{

  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "gnome3";
    enableZshIntegration = true;
  };

  home.packages = [ pkgs.pinentry-gnome ];
}
