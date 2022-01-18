{ inputs, pkgs, ... }:
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.reuben = {
    imports = [
      inputs.nix-doom-emacs.hmModule
      ./home.nix
    ];
  };
}
