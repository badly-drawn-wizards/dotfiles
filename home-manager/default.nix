{ inputs, pkgs, ... }:
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.reuben = {
    imports = [
      inputs.nix-doom-emacs-unstraightened.hmModule
      inputs.nixvim.homeManagerModules.nixvim
      inputs.hyprland.homeManagerModules.default
      inputs.nix-index-database.hmModules.nix-index
      ./home.nix
    ];
  };
}
