{ config, lib, pkgs, ... }:

# This whole thing is just me throw shit at the wall
# and seeing what sticks.
#
# Later I'll get into understanding MESA better.

{
    # https://nixos.wiki/wiki/Intel_Graphics
    # TODO Get vainfo to work with iris
    hardware.opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
      ];
      extraPackages32 = with pkgs.pkgsi686Linux; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
      ];
    };

    environment.variables = {
      # # TODO Figure out how to get mesa iris drivers working.
      # MESA_LOADER_DRIVER_OVERRIDE = "iris";
    };
}
