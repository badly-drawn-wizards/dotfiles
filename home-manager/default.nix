{ inputs, pkgs, ... }:
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.backupFileExtension = ".bak";
  home-manager.users.reuben = {
    imports = [
      inputs.nixvim.homeModules.nixvim
      inputs.nix-index-database.homeModules.nix-index
      ./home.nix
    ];
  };
}
