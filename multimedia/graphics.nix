{ config, lib, pkgs, ... }:

{
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; with rocmPackages; [
      amdvlk
      clr
      clr.icd
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      driversi686Linux.amdvlk
    ];
  };
}
