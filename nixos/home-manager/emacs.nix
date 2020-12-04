{ config, lib, pkgs, inputs, ... }:

{
  imports = [inputs.nix-doom-emacs.hmModule];

  home.sessionVariables.EDITOR = "em";
  home.file = {
    ".local/bin/em".source =
      let
        em-path =
          pkgs.writeScriptBin "em" ''
            #!/usr/bin/env /bin/sh
            emacsclient -c -a "" $@
          '';
      in
      "${em-path}/bin/em";
  };

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./doom;
    emacsPackage = pkgs.emacsGit;
  };

  home.packages = with pkgs; [
    # So emacs can compliment me
    espeak
  ];
}
