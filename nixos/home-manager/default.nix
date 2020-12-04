{ home-manager }:
{
  imports = [home-manager.nixosModules.home-manager];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.reuben = import ./home-manager;
}
