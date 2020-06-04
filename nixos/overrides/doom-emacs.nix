self: super:

{
  # TODO change this to use niv
  doom-emacs = self.callPackage
    (builtins.fetchTarball
      {
        url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
      })
    {
      doomPrivateDir = ./doom;
      extraPackages = epkgs: [ epkgs.doom-themes ];
    };
}
