{ pkgs, lib, ... }:

let
  usePipewire = true;
  usePulseaudio = false;
in
{
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = !usePulseaudio;

    wireplumber.enable = true;
  };

  hardware.pulseaudio = lib.mkIf usePulseaudio {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };

}
