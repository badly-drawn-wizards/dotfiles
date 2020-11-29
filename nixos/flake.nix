{
  outputs = {self, nixpkgs}: {
    nixosConfigurations.noobnoob = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ import ./configuration.nix ];
    };
  };
}
