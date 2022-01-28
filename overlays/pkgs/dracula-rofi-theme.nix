{ stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation {
  name = "dracula-rofi-theme";

  src = fetchFromGitHub {
    owner = "dracula";
    repo = "rofi";
    rev = "6d92bc04ba01655fb3645dabeecdfe38f59969a6";
    sha256 = "JOfgzFRcZwPC5PnkwOW3jVDMuJv+svTXtg0CthKM1sA=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p "$out"
    mv theme $out/

    runHook postInstall
  '';
}
