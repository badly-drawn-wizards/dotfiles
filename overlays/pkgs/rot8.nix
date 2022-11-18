{ lib, rustPlatform, fetchFromGitHub, procps, sway, makeWrapper }:
rustPlatform.buildRustPackage {
  pname = "rot8";
  version = "0.1.5";

  propagatedBuildInputs = [ procps sway ];
  nativeBuildInputs = [ makeWrapper ];

  cargoSha256 = "Zz3RK79pMBn9JcpOVHf8vrvQzOJuV7anm136HcTBhJE=";

  src = fetchFromGitHub {
    owner = "efernau";
    repo = "rot8";
    rev = "0e737607ac4977e209988e0816143778e3446fb6";
    sha256 = "i+VLVA/XKZiFPEeFHR3CpZKi8CWA/tiaZJerciqQHJ0=";
    fetchSubmodules = true;
  };

  postInstall = ''
    wrapProgram "$out/bin/rot8" \
      --prefix PATH : ${lib.makeBinPath [procps sway]}
  '';
}
