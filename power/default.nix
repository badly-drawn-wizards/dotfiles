{ config, lib, pkgs, ... }:

{
  services.upower = {
    enable = true;
    criticalPowerAction = "HybridSleep";
  };
  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleLidSwitch=hybrid-sleep
    IdleAction=suspend
    IdleActionSec=10m
  '';

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=15m
  '';

}
