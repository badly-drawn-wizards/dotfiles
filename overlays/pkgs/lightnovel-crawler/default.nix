{ pkgs, mach-nix, fetchFromGitHub, applyPatches }:

mach-nix.buildPythonApplication rec {
  pname = "lightnovel-crawler";
  version = "2.29.7";
  src = fetchFromGitHub {
    owner = "dipu-bd";
    repo = pname;
    rev = "v${version}";
    sha256 = "vd9CMh43Mig0PU+/71eJYV4uis9ssEfYH0cllAUj0fQ=";
  };
}
