{ config, lib, pkgs, ... }:

# This whole thing is just me throw shit at the wall
# and seeing what sticks.
#
# Later I'll get into understanding MESA better.

{
  # https://nixos.wiki/wiki/Intel_Graphics

  environment.variables = {
    MESA_LOADER_DRIVER_OVERRIDE = "iris";
  };
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    package = (pkgs.mesa.override {
      galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
    }).drivers;
    package32 = (pkgs.pkgsi686Linux.mesa.override {
      galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
    }).drivers;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

}
