{ config, lib, pkgs, ... }:

{
  hardware = {
    sane = {
      enable = true;
      extraBackends = [ pkgs.hplipWithPlugin ];
    };
  };

  services.printing = {
    enable = true;
    drivers = with pkgs; [
      gutenprint
      gutenprintBin
    ];
  };
}
