let
  sources = import /workspace/dotfiles/nix/sources.nix;
  niv-pkgs = import sources.nixpkgs {};
  # nix-doom-emacs = import sources.nix-doom-emacs;
  nix-doom-emacs = import /workspace/nix-doom-emacs;
in
{
  allowUnfree = true;

  packageOverrides = pkgs: {
    inherit niv-pkgs;
    doom-emacs = pkgs.callPackage nix-doom-emacs {
      doomPrivateDir = /workspace/dotfiles/doom;
      dependencyOverrides = {
        inherit (sources) emacs-overlay doom-emacs;
      };
    };
  };
}
