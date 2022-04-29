self: super:

{
  rtags = super.rtags.overrideAttrs (attrs: rec {
    version = "3.23";
    src = self.fetchFromGitHub {
      owner = "andersbakken";
      repo = "rtags";
      rev = "v${version}";
      sha256 = "sha256-EqtvSBCaet4Fm8Aj4tPANR7E8hHi/pv/CdN8Y4wQYDU=";
      fetchSubmodules = true;
      # unicode file names lead to different checksums on HFS+ vs. other
      # filesystems because of unicode normalisation
      postFetch = ''
        rm $out/src/rct/tests/testfile_*.txt
      '';
    };
  });
}
