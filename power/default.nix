{ config, lib, pkgs, ... }:

{
  services.upower = {
    enable = true;
    criticalPowerAction = "Hibernate";
  };
  #HandleLidSwitch=suspend-then-hibernate
  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleLidSwitch=suspend
    IdleAction=lock
    IdleActionSec=300
  '';

  # systemd.sleep.extraConfig = ''
  #   HibernateDelaySec=900
  # '';

}
