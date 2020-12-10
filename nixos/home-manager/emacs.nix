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
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ../doom;
    emacsPackage = pkgs.emacsGit;
  };

  home.packages = with pkgs; [
    # So emacs can compliment me
    espeak
    ripgrep
  ];
}
