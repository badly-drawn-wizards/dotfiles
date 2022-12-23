{ config, lib, pkgs, ... }:

{
  services.upower = {
    enable = true;
    criticalPowerAction = "Hibernate";
  };
  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleLidSwitch=suspend-then-hibernate
    IdleAction=lock
    IdleActionSec=10m
  '';

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=15m
  '';

}
