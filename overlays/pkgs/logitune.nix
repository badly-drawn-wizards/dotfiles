{ lib
, stdenv
, fetchFromGitHub
, cmake
, ninja
, pkg-config
, qt6
, systemd
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "logitune";
  version = "0.3.7";

  src = fetchFromGitHub {
    owner = "mmaher88";
    repo = "logitune";
    rev = "v${finalAttrs.version}";
    hash = "sha256-kjUerrbJQ/KMfvdvwB6Ix8FCqF3fOA6lDFureKkdO+o=";
  };

  nativeBuildInputs = [
    cmake
    ninja
    pkg-config
    qt6.qttools
    qt6.wrapQtAppsHook
  ];

  buildInputs = [
    qt6.qtbase
    qt6.qtdeclarative
    qt6.qtsvg
    qt6.qt5compat
    systemd
  ];

  cmakeFlags = [
    # The source tarball has no .git, so CMake's git-describe lookup fails
    # and the configure step refuses to guess. Pass the version explicitly.
    (lib.cmakeFeature "LOGITUNE_VERSION" finalAttrs.version)
    (lib.cmakeBool "BUILD_TESTING" false)
  ];

  # CMakeLists.txt installs the autostart desktop file to an absolute
  # /etc/xdg/autostart path, which is unwritable in the Nix sandbox.
  # Redirect it under $out/etc so the install step succeeds.
  postPatch = ''
    substituteInPlace CMakeLists.txt \
      --replace-fail "DESTINATION /etc/xdg/autostart" "DESTINATION \''${CMAKE_INSTALL_SYSCONFDIR}/xdg/autostart"
  '';

  meta = {
    description = "Logitech device configurator for Linux — per-app profiles, button remapping, DPI, gestures";
    homepage = "https://github.com/mmaher88/logitune";
    license = lib.licenses.gpl3Plus;
    platforms = lib.platforms.linux;
    mainProgram = "logitune";
  };
})
