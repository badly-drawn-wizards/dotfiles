{pkgs, lib, stdenv, makeWrapper, python3, python3Packages, pulseaudio, xdotool, python-vosk, ... }:
stdenv.mkDerivation rec {
    name = "nerd-dictation";
    buildInputs = [ makeWrapper python3 python3Packages.wrapPython];
    runtimeDeps = [ pulseaudio xdotool ];
    pythonDeps = [ python-vosk ];
    src = pkgs.fetchFromGitHub {
      owner = "ideasman42";
      repo = "nerd-dictation";
      rev = "9e0806f4fa8826ce943b15e6f62918a78ffa90d2";
      sha256 = "09hlb1gnrxmgnjv3qgq26zxalap20ybwbc57brfnpnjcaxsss9ac";
    };
    dontBuild = true;

    installPhase = ''
    chmod +x nerd-dictation
    cp nerd-dictation $out/bin/
    wrapProgram "$out/bin/nerd-dictation" \
      --prefix PYTHONPATH : $PYTHONPATH
      --prefix PATH : "${lib.makeBinPath runtimeDeps}"
    '';
}
