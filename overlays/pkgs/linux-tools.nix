{ stdenv, linux, libuuid }:

linux.overrideAttrs (attrs: {
  buildInputs = (attrs.buildInputs or []) ++ [ libuuid ];
  buildFlags = [
    "KBUILD_BUILD_VERSION=1-NixOS"
    "tools/acpi"
  ];
  installFlags = [
    "INSTALL_PATH=$(out)"
    "DESTDIR=$(out)"
  ];
  installTargets = ["tools/acpi_install"];
  postPatch = ''
    ${attrs.postPatch}
    for file in $(find tools -name 'Makefile*')
    do
      substituteInPlace "$file" \
        --replace "/bin/true" "true" \
        --replace "/bin/false" "false" \
        --replace "/usr/bin/install" "install"
    done
  '';
  postInstall = ''
  mv $out/usr/* $out/
  rm -r $out/usr
  '';
})
