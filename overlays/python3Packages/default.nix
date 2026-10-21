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
      # fabric (needed by azure-cli) requires invoke<3. Pinned for the whole
      # set because paramiko depends on invoke too, and two versions in one
      # closure conflict. TODO: drop once fabric supports invoke 3
      # (https://github.com/fabric/fabric/issues/2376).
      invoke = psuper.invoke.overridePythonAttrs (old: rec {
        version = "2.2.1";
        src = pself.fetchPypi {
          pname = "invoke";
          inherit version;
          hash = "sha256-UVv0m0pIkyt5sCRZA0jaIvOcSULf+ZGtH7i4uuob5wc=";
        };
        # nixpkgs' test setup targets invoke 3.
        doCheck = false;
      });

      # Asserts an exact TypeError message that newer Python changed.
      pyopenjtalk-plus = psuper.pyopenjtalk-plus.overridePythonAttrs (old: {
        disabledTests = (old.disabledTests or [ ]) ++ [
          "test_run_njd_from_mecab_invalid_input_should_not_break_next_call"
        ];
      });
    })
  ];
}
