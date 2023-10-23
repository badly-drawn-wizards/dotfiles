{ pkgs, lib, config, ... }:

{
  services.kanshi = {
    enable = true;
    profiles = {
      undocked.outputs = [
        { criteria = "eDP-1"; }
      ];
      docked-david.outputs = [
        { 
          criteria = "eDP-1"; 
          position = "340,1440";
        }
        { 
          criteria = "Dell Inc. DELL U2717D J0XYN952A8ML";
        }
      ];
    };
  };
}

