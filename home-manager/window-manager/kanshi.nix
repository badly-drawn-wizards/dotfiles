{ pkgs, lib, config, ... }:

{
  services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "undocked";
        profile.outputs = [
          { criteria = "eDP-1"; }
        ];
      }

      {
        profile.name = "docked-wide";
        profile.outputs = [
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
      }

      {
        profile.name = "docked-work";
        profile.outputs = [
          {
            criteria = "Dell Inc. DELL S2722DC 49Q4HD3";
            position = "0,0";
            mode = "2560x1440@59.951000Hz";
          }
          {
            criteria = "eDP-1";
            position = "320,1440";
          }
        ];
      }
    ];
  };
}

