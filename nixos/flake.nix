{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  outputs = { nixpkgs, ... }@inputs: {
    nixosConfigurations.noobnoob = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        {
          specialArgs = {
            inherit inputs;
          };
        }
        ./configuration.nix
      ];
    };
  };
}
