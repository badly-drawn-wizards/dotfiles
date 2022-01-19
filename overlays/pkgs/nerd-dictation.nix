{pkgs, lib, stdenv, makeWrapper, python3, python3Packages, pulseaudio, xdotool,
  python-vosk, glib, glibc, vosk-api, ... }:
stdenv.mkDerivation rec {
    name = "nerd-dictation";
    buildInputs = [ makeWrapper python3 python3Packages.wrapPython];
    propagatedBuildInputs = [ python-vosk ];
    runtimeDeps = [ pulseaudio xdotool ];

    libPath = lib.makeLibraryPath ([ glib glibc vosk-api ]);
    src = pkgs.fetchFromGitHub {
      owner = "ideasman42";
      repo = "nerd-dictation";
      rev = "9e0806f4fa8826ce943b15e6f62918a78ffa90d2";
      sha256 = "09hlb1gnrxmgnjv3qgq26zxalap20ybwbc57brfnpnjcaxsss9ac";
    };
    dontBuild = true;

    installPhase = ''
    mkdir -p $out/bin
    chmod +x nerd-dictation
    cp nerd-dictation $out/bin/
    wrapProgram "$out/bin/nerd-dictation" \
      --prefix PYTHONPATH : $PYTHONPATH \
      --prefix LD_LIBRARY_PATH : ${libPath} \
      --prefix PATH : "${lib.makeBinPath runtimeDeps}"
    '';
}
