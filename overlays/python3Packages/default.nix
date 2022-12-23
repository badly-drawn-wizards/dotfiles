self: super:
let inherit (self) callPackage; in
{
  python3 = super.python3.override {
    packageOverrides = pself: psuper: {
      diffusers = super.mach-nix.mkPython {
        requirements = ''
        diffusers
        distlib==0.3.6
        '';

        providers = {
          _default = "nixpkgs";
          distlib = "nixpkgs";
        };

      };
    };
  };
}
