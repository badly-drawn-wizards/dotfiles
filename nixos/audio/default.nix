{ config, lib, pkgs, ... }:

{

  # Try get internal microphone working
  # boot.kernelPatches = [{
  #   name = "skylake-hdaudio-codec";
  #   patch = null;
  #   extraConfig = ''
  #     SND_SOC_INTEL_SKYLAKE_HDAUDIO_CODEC y
  #   '';
  # }];

  sound = {
    enable = true;
  };

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };


}
