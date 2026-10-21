self: super:
let inherit (self) callPackage; in
{
  python3 = super.python3.override {
    packageOverrides = pself: psuper: {
    };
  };

  # Fixes for every Python package set (composes with other overrides,
  # unlike python3.override packageOverrides).
  pythonPackagesExtensions = super.pythonPackagesExtensions ++ [
    (pself: psuper: {
      # fabric 3.2.3 requires invoke<3 but nixpkgs moved to invoke 3, so it is
      # marked broken and azure-cli fails its runtime deps check. Pin invoke
      # back to 2.2.1 for the whole set: paramiko (also used by azure-cli)
      # depends on invoke too, and a fabric-only copy duplicates it in the
      # closure. fabric's broken flag is computed from invoke's version, so it
      # clears. Rebuilds paramiko/scp/sshtunnel/fabric/azure-cli locally.
      # TODO: drop once fabric supports invoke 3
      # (https://github.com/fabric/fabric/issues/2376).
      invoke = psuper.invoke.overridePythonAttrs (old: rec {
        version = "2.2.1";
        src = pself.fetchPypi {
          pname = "invoke";
          inherit version;
          hash = "sha256-UVv0m0pIkyt5sCRZA0jaIvOcSULf+ZGtH7i4uuob5wc=";
        };
        # The 3.x test suite doesn't match the 2.2.1 sources.
        doCheck = false;
      });

      # One test asserts an exact TypeError message that changed in Python
      # 3.14. Breaks piper-tts, wyoming-piper, speech-dispatcher, calibre.
      pyopenjtalk-plus = psuper.pyopenjtalk-plus.overridePythonAttrs (old: {
        disabledTests = (old.disabledTests or [ ]) ++ [
          "test_run_njd_from_mecab_invalid_input_should_not_break_next_call"
        ];
      });
    })
  ];
}
