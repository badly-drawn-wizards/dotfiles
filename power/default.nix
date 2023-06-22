{ config, lib, pkgs, ... }:

{
  services.upower = {
    enable = true;
    criticalPowerAction = "HybridSleep";
  };
  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleLidSwitch=hybrid-sleep
    IdleAction=lock
    IdleActionSec=5m
  '';

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=15m
  '';

}
