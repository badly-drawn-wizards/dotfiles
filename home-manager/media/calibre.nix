{ config, lib, pkgs, ... }:

{
  programs.calibre = {
    enable = true;
    # Read-aloud pulls piper-tts, which needs torch built from source.
    # TODO: drop the override once torch is in the binary cache.
    package = pkgs.calibre.override { speechSupport = false; };
  };

  home.sessionVariables.CALIBRE_USE_DARK_PALETTE = "1";
}
