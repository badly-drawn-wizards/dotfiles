{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "/nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "/nixpkgs";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "/nixpkgs";
    nix-doom-emacs.inputs.emacs-overlay.follows = "/emacs-overlay";
  };
  outputs = { nixpkgs, ... }@inputs: {
    nixosConfigurations.noobnoob = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
      };
      modules = [
        {
          nix = {
            package = nixpkgs.nixFlakes;
            extraOptions = ''
              experimental-features = nix-command flakes
            '';
            registry.nixpkgs.flake = inputs.nixpkgs;
          };
        }
        ./configuration.nix
      ];
    };
  };
}
