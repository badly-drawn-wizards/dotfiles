self: super: with self; {
  libcyaml = stdenv.mkDerivation {
    pname = "libcyaml";
    version = "1.1.0";
    src = fetchFromGitHub {
      owner = "tlsa";
      repo = "libcyaml";
      rev = "7a443463d7676e5336061614305fa45d0b9a3cba";
      sha256 = "0428p0rwq71nhh5nzcbapsbrjxa0x5l6h6ns32nxv7j624f0zd93";
    };

    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ libyaml ];

    makeFlags = [ "PREFIX=${placeholder "out"}" ];

  };

  sphinx-intl = python38.pkgs.buildPythonPackage rec {
    pname = "sphinx-intl";
    version = "2.0.1";

    src = python38.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "b25a6ec169347909e8d983eefe2d8adecb3edc2f27760db79b965c69950638b4";
    };

    propagatedBuildInputs = with python38Packages; [ sphinx click Babel ];
  };

  libaudec = stdenv.mkDerivation rec {
    pname = "libaudec";
    version = "0.2.4";

    src = fetchFromGitHub  {
      owner = "zrythm";
      repo = "libaudec";
      rev = "b6f2ceb3041d097eb9225e974bb200187f00a454";
      sha256 = "1570m2dfia17dbkhd2qhx8jjihrpm7g8nnyg6n4wif4vv229s7dz";
    };

    nativeBuildInputs = [ meson ninja pkg-config ];

    buildInputs = [
      libsndfile
      libsamplerate
    ];
  };

  zrythm = stdenv.mkDerivation {
    pname = "zrythm";
    version = "1.0.0-alpha.5.0.1";

    src = fetchFromGitHub {
      owner = "zrythm";
      repo = "zrythm";
      rev = "cc8be4f7c5f97198cabaeba3862d15177137096d";
      sha256 = "1dvc3yqfa8jdjqia9rvjc27f29g4jkvpmjcg35qw1fjsz5g96lzk";
    };

    nativeBuildInputs = [
      meson
      cmake
      pkg-config
    ];

    buildInputs = [
      gettext
      graphviz
      gtk3
      guile
      help2man
      libaudec
      libjack2
      libcyaml
      libyaml
      pandoc
      python38Packages.sphinx
      sphinx-intl
      texi2html
      xdg_utils
    ];

  };
}
