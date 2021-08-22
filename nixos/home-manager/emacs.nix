{ config, lib, pkgs, inputs, ... }:

{
  home.sessionVariables.EDITOR = "em";
  home.file = {
    ".local/bin/em" = {
      text = ''
        #!/usr/bin/env /bin/sh
        emacsclient -c -a "" $@
      '';
      executable = true;
    };
  };

  # A marriage made in hell
  programs.doom-emacs =
    let
      emacs = pkgs.emacs;
      epkgs = pkgs.emacsPackagesFor emacs;
    in {
      enable = true;
      doomPrivateDir = ../doom;
      extraPackages = [ epkgs.lean4-mode ];
      emacsPackage = emacs;
    };

  home.packages = with pkgs; [
    # So emacs can compliment me
    espeak
    ripgrep
  ];
}
