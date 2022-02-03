{ config, lib, pkgs, ... }:

{
  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleLidSwitch=suspend-then-hibernate
  '';

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=30m
  '';
}
