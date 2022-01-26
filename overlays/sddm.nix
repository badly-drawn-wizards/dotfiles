self: super:

{
  sddm = super.sddm.override rec {
    version = "0.2.0";
    src = super.fetchFromGitHub {
      owner = "sddm";
      repo = "sddm";
      rev = "v${version}";
      sha256 = "1s6icb5r1n6grfs137gdzfrcvwsb3hvlhib2zh6931x8pkl1qvxa";
    };
  };
}
