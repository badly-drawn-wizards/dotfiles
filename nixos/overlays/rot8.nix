self: super: {
  rot8 = super.rustPlatform.buildRustPackage {
    pname = "rot8";
    version = "0.1.3";

    cargoSha256 = "0wbkpv4jf4f8pdkfha2v4y5qc5rwfh8l6g2a812nsx0j1yw9z0d5";

    src = super.fetchFromGitHub {
      owner = "efernau";
      repo = "rot8";
      rev = "8f2128c172be8ecc3e76e0801534413b301ccbd2";
      sha256 = "0p86ph7yc0flixpjbyl3p7kbph9x86k7xrp04m1i5y43l7yrywmm";
    };
  };
}
