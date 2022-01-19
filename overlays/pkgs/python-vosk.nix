{ pkgs, python3Packages, vosk-api, ... }:

let
  repo = pkgs.fetchFromGitHub (import ./vosk-repo.nix);
in
python3Packages.buildPythonPackage rec {

  pname = "python-vosk";
  version = "0.3.32";
  src = "${repo}/python";
  patches = [ ./python-vosk-dynamic.patch ];

  doCheck = false;
  propagatedBuildInputs = [ python3Packages.cffi vosk-api ];
  VOSK_API_H = "${vosk-api}/include/vosk_api.h";
}
