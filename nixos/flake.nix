{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  outputs = {self, nixpkgs, home-manager, nix-doom-emacs, emacs-overlay, ...}: {
    nixosConfigurations.noobnoob = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          nixpkgs.overlays = [emacs-overlay.overlay];
        }
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.reuben = import ./home-manager ;
        }
        {
          home-manager.users.reuben = {pkgs, ...}: {
            imports = [nix-doom-emacs.hmModule];
            programs.doom-emacs = {
              enable = true;
              doomPrivateDir = ./doom;
              emacsPackage = pkgs.emacsGcc;
            };
          };
        }
      ];
    };
  };
}
