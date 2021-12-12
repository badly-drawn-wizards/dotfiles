{ config, lib, pkgs, ... }:

{
  sound = {
    enable = true;
  };

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };

  services.pipewire.enable = true;
}
