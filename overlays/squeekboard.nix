self: super: with super; {
  squeekboard = stdenv.mkDerivation rec {
    pname = "squeekboard";
    version = "1.10.0";

    cargoSha256 = null;

    src = fetchFromGitLab {
      domain = "source.puri.sm";
      owner = "Librem5";
      repo = pname;
      rev = "v${version}";
      sha256 = "029hbiqfbyqsjx2p9hnqf9dxvfirkb545zrsdjdvrcv36p9dd3vx";
    };

    nativeBuildInputs = [
      meson
      cmake
      ninja
      pkg-config
      rustc
      cargo
      glib  # for glib-compile-resources
      wayland
      makeWrapper
    ];

    buildInputs = [
      gtk3  # for gio-2.0
      gnome3.gnome-desktop
      wayland
      wayland-protocols
      libxml2
      libxkbcommon
    ];

  #   # Don't use buildRustPackage phases, only use it for rust deps setup
  #   configurePhase = '';
  #   buildPhase = '';
  #   checkPhase = '';
  #   installPhase = '';

  };
}
