let
  sources = import /workspace/dotfiles/nixos/nix/sources.nix;
in
{
  allowUnfree = true;

  packageOverrides = pkgs: {
    niv-pkgs = import sources.nixpkgs {};
    doom-emacs = pkgs.callPackage
      (builtins.fetchTarball
        {
          url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
        })
      {
        doomPrivateDir = /workspace/dotfiles/nixos/home-manager/doom;
        extraPackages = epkgs: [ epkgs.doom-themes ];
      };
  };
}
