{ config, lib, pkgs, ... }:

{
  environment.variables = {
    # MESA_LOADER_DRIVER_OVERRIDE = "iris";
    VDPAU_DRIVER = "va_gl";
  };
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      libva
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      libva
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  hardware.video.hidpi.enable = true;
}
