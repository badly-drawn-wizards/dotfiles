{ config, lib, pkgs, ... }:

{
    # From
    # https://nixos.wiki/wiki/Intel_Graphics
    # TODO Get vainfo to work with iris
    hardware.opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        libva-full
        intel-media-driver
        intel-compute-runtime

        vaapiVdpau
        vaapiIntel
        vaapi-intel-hybrid
        libvdpau-va-gl
      ];
      package = (pkgs.mesa.override {
        galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
      }).drivers;
    };

    environment.variables = {
      MESA_LOADER_DRIVER_OVERRIDE = "iris";
    };
}
