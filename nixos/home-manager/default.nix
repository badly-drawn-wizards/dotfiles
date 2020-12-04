{ inputs, ... }:
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.reuben = ./home-manager.nix;
}
