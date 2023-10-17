{ pkgs, lib, ... }:

let
  usePipewire = true;
in
{
  services.pipewire = {
    enable = true;
    audio.enable = true;
    # alsa.enable = true;
    # alsa.support32Bit = true;
    pulse.enable = true;

    wireplumber.enable = true;
  };

  # hardware.pulseaudio = lib.mkIf usePulseaudio {
  #   enable = true;
  #   extraModules = [ pkgs.pulseaudio-modules-bt ];
  #   package = pkgs.pulseaudioFull;
  #   support32Bit = true;
  # };

  environment.etc."pipewire/pipewire.d/echo-cancel.conf".text = ''
    context.modules = [
      {
        name = libpipewire-module-echo-cancel
        args = {
          capture.props = {
            node.name = "Echo Cancellation Capture"
          }
          source.props = {
            node.name = "Echo Cancellation Source"
          }
          sink.props = {
            node.name = "Echo Cancellation Sink"
          }
          playback.props = {
            node.name = "Echo Cancellation Playback"
          }
        }
      }
    ]
  '';

}
