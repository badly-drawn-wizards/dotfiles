{ pkgs, lib, config, ... }:

{
  services.kanshi = {
    enable = true;
    profiles = {

      undocked.outputs = [
        { criteria = "eDP-1"; }
      ];

      docked.outputs = [
        {
          criteria = "Dell Inc. DELL S3422DW CVD1TH3";
          position = "0,0";
          mode = "3440x1440@99.982Hz";
        }
        {
          criteria = "eDP-1";
          position = "750,1440";
        }
      ];

      docked-david.outputs = [
        {
          criteria = "Dell Inc. DELL U2717D J0XYN952A8ML";
          position = "0,0";
          mode = "2560x1440@59.951Hz";
        }
        {
          criteria = "eDP-1";
          position = "340,1440";
        }
      ];
    };
  };
}

