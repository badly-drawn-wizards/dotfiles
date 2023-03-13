self: super:
let inherit (self) callPackage; in
{
  python3 = super.python3.override {
    packageOverrides = pself: psuper: {
    };
  };
}
