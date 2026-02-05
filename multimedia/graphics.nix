{ config, lib, pkgs, ... }:

{
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; with rocmPackages; [
      clr
      clr.icd
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
    ];
  };
}
