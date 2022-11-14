{ stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation {
  name = "dracula-rofi-theme";

  src = fetchFromGitHub {
    owner = "dracula";
    repo = "rofi";
    rev = "090a990c8dc306e100e73cece82dc761f3f0130c";
    sha256 = "raoJ3ndKtpEpsN3yN4tMt5Kn1PrqVzlakeCZMETmItw=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p "$out"
    mv theme $out/

    runHook postInstall
  '';
}
