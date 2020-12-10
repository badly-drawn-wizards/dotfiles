{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
      };
    };
    nur = {
      url = "github:nix-community/NUR";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
      };
    };
    nix-doom-emacs = {
      url = "github:vlaci/nix-doom-emacs";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
        emacs-overlay.follows = "/emacs-overlay";
      };
    };
    flake-compat = {
        url = "github:edolstra/flake-compat";
        flake = false;
    };
  };
  outputs = { nixpkgs, ... }@inputs: {
    nixosConfigurations.noobnoob = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
      };
      modules = [
        ./configuration.nix
      ];
    };
  };
}
