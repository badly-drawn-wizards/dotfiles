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
  outputs = inputs: {
    nixosConfigurations.noobnoob = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
      };
      modules = [
        ./configuration.nix
        ./home-manager
        ({...}: {
          home-manager.users.reuben.config = {
            _module.args.inputs = inputs;
          };
        })
      ];
    };
  };
}
