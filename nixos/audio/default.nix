{ config, lib, pkgs, ... }:

{

  # Try get internal microphone working
  boot.kernelPatches = [{
    name = "skylake-hdaudio-codec";
    patch = null;
    extraConfig = ''
      SND_SOC_INTEL_SKYLAKE_HDAUDIO_CODEC y
    '';
  }];

  sound = {
    enable = true;
  };

  environment.systemPackages = with pkgs; [
    alsa-firmware
    niv-pkgs.alsa-ucm-conf
    niv-pkgs.alsa-topology-conf
  ];

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };


}
