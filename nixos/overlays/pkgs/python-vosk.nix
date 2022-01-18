{ pkgs, python3Packages, vosk-api, ... }:

let
  repo = pkgs.fetchFromGitHub (import ./vosk-repo.nix);
in
python3Packages.buildPythonPackage rec {
  pname = "python-vosk";
  version = "0.3.32";
  src = "${repo}/python";
  propagatedBuildInputs = [ python3Packages.cffi vosk-api ];
}
