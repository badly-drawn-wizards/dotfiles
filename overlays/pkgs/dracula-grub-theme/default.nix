{ stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation {
  name = "dracula-grub-theme";

  src = fetchFromGitHub {
    owner = "dracula";
    repo = "grub";
    rev = "0f7978034cd2cd84da5aefe72e8d928ec7667fbe";
    sha256 = "147zzifs02d78gl2ig5fi1rzl04w367wj7khw23b896ndhjmdzfv";
  };

  buildPhase = ''
  cp ${./wizard}/* dracula/
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out"
    mv dracula/* $out/

    runHook postInstall
  '';
}
