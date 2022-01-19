{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    #nerd-dictation
  ];

  home.file = {
    ".config/nerd-dictation/model" = {
      source = pkgs.vosk-model-small-en-us;
    };
  };
}
