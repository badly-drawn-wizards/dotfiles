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
    IdleActionSec=300
  '';

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=900
  '';

}
