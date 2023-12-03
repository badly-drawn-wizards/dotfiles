{ stdenv
, lib
, buildGoModule
, fetchFromGitHub
, writeScriptBin
, go
, yq-go
, virtiofsd
, qemu_kvm
, makeWrapper
}:

buildGoModule rec {
  pname = "kata-runtime";
  version = "3.2.0";

  # https://github.com/NixOS/nixpkgs/issues/25959

  src = fetchFromGitHub {
    owner = "kata-containers";
    repo = "kata-containers";
    rev = version;
    sha256 = "sha256-zEKuEjw8a5hRNULNSkozjuHT6+hcbuTIbVPPImy/TsQ=";
  };

  sourceRoot = "source/src/runtime";

  vendorHash = null;

  dontConfigure = true;

  makeFlags = [
    "SKIP_GO_VERSION_CHECK=1"
    "PREFIX=${placeholder "out"}"
    "DEFAULT_HYPERVISOR=qemu"
    "HYPERVISORS=qemu"
    "QEMUPATH=${qemu_kvm}/bin/qemu-system-x86_64"
  ];

  buildInputs = [ yq-go ];

  buildPhase = ''
    runHook preBuild
    mkdir -p $TMPDIR/gopath
    ln -s ${yq-go}/bin $TMPDIR/gopath
    HOME=$TMPDIR GOPATH=$TMPDIR/gopath make ${toString makeFlags}
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    HOME=$TMPDIR GOPATH=$TMPDIR/gopath make ${toString makeFlags} install
    ln -s $out/bin/containerd-shim-kata-v2 $out/bin/containerd-shim-kata-qemu-v2
    ln -s $out/bin/containerd-shim-kata-v2 $out/bin/containerd-shim-kata-clh-v2

    # qemu images don't work on read-only mounts, we need to put it into a mutable directory
    sed -i \
      -e "s!$out/share/kata-containers!/var/lib/kata-containers!" \
      -e "s!^virtio_fs_daemon.*!virtio_fs_daemon=\"${virtiofsd}/bin/virtiofsd\"!" \
      -e "s!^valid_virtio_fs_daemon_paths.*!valid_virtio_fs_daemon_paths=[\"${virtiofsd}/bin/virtiofsd\"]!" \
      "$out/share/defaults/kata-containers/"*.toml


    runHook postInstall
  '';

  meta = {
    description = "Container runtime based on lightweight virtual machines";
    homepage = "https://github.com/kata-containers/kata-containers";
    license = lib.licenses.asl20;
    platforms = lib.platforms.unix;
  };
}
