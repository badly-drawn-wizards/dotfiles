self: {pkgs, lib, stdenv, python3Packages, ...}@super:
{
  vosk-model-small-en-us = stdenv.mkDerivation {
    name = "vosk-model-small-en-us";
    src = pkgs.fetchzip {
      url = "https://alphacephei.com/vosk/models/vosk-model-small-en-us-0.15.zip";
      sha256 = "sha256-CIoPZ/krX+UW2w7c84W3oc1n4zc9BBS/fc8rVYUthuY=";
    };
    dontBuild = true;
    installPhase = ''
    mkdir -p $out
    cp -R ./* $out
    '';
  };

  nerd-dictation = pkgs.callPackage (import ./pkgs/nerd-dictation.nix) {};
  vosk-api = pkgs.callPackage (import ./pkgs/vosk-api.nix) {};
  python-vosk = pkgs.callPackage (import ./pkgs/python-vosk.nix) {};

}
