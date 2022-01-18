let
  komikku =
    { lib
    , stdenv
    , ninja
    , meson
    , cmake
    , pkg-config
    , python3
    , python3Full
    , glib, glibc, gobject-introspection, wrapGAppsHook
    , gtk3
    , libhandy
    , fetchFromGitLab
    , ...
    }: stdenv.mkDerivation rec {

      pname = "komikku";
      version = "0.36.0";
      src = fetchFromGitLab {
        owner = "valos";
        repo = "Komikku";
        rev = "0d9d5715dbd3f1a0ac82126176b6edcab63d894a";
        sha256 = "sha256-0ucSXKgygMJhhb8g9N9rPtb9PgTI8Ba2a4pieFso7BU=";
      };

      strictDeps = false;
      buildInputs = [
        gtk3
        gobject-introspection
      ];


      nativeBuildInputs =
        [
          meson
          ninja
          cmake
          pkg-config
          python3
          glib
          glibc
          gobject-introspection
          wrapGAppsHook
          libhandy
        ];

      propagetedBuildInputs = with python3.pkgs; [
        beautifulsoup4
        brotli
        cloudscraper
        dateparser
        keyring
        lxml
        magic
        natsort
        pillow
        pure-protobuf
        unidecode
      ];
    };
in
self: super: {
  python3 = super.python3.override {
    packageOverrides = self: super: {
      pure-protobuf =
        super.buildPythonPackage rec {
          pname = "pure-protobuf";
          version = "2.0.1";
          src = super.fetchPypi {
            inherit pname version;
            sha256 = "sha256-fIFsPQwWP9nk/fbVVV1TAaujvURFd3FzWlaOWdw3+QE=";
          };
        };
    };
  };
  komikku = super.callPackage komikku { };
}
