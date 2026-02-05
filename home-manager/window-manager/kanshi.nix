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
    ];
  };
}

