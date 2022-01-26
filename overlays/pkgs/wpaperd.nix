{ fetchFromGitHub, rustPlatform, ... }:
rustPlatform.buildRustPackage rec {
  name = "wpaperd";
  version = "0.1.0";
  src = fetchFromGitHub {
    owner = "danyspin97";
    repo = "wpaperd";
    rev = version;
    sha256 = "1kpy4mx6xf0h4x9gdmpdn062kmgxy9ccrjiz8vb5qy73k4g669lj";
  };

  cargoSha256 = "i2EbQLP6HG0neHVwevtDKwDowBXMDvm8l7bS+ouUizo=";
}
