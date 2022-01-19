{ pkgs, stdenv, openblas, openfst, kaldi, mkl ? null, ... }:
let
  repo = pkgs.fetchFromGitHub (import ./vosk-repo.nix);
in
stdenv.mkDerivation {
  name = "vosk-api";
  version = "0.3.32";

  KALDI_ROOT = kaldi;
  OPENFST_ROOT = openfst;
  OPENBLAS_ROOT = openblas;
  HAVE_MKL = if mkl == null then 0 else 1;

  buildInputs = [ kaldi openfst openblas mkl ];

  src = "${repo}/src";
  patches = [ ./vosk-api-dynamic.patch ];
  installPhase = ''
    mkdir -p $out/{lib,include/vosk}
    cp *.so $out/lib/
    cp *.h  $out/include/vosk/
  '';
}
