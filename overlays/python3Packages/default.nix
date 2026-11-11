self: super:
let inherit (self) callPackage; in
{
  python3 = super.python3.override {
    packageOverrides = pself: psuper: { };
  };

  # Fixes for every Python package set (composes with other overrides,
  # unlike python3.override packageOverrides).
  pythonPackagesExtensions = super.pythonPackagesExtensions ++ [
    (pself: psuper: { })
  ];
}
