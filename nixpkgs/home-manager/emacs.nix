{ config, lib, pkgs, ... }:

{
  home.sessionVariables.EDITOR = "em";
  home.file = {
    ".local/bin/em".source =
      let
        em-path =
          pkgs.writeScriptBin "em" ''
            #!/bin/sh
            ${pkgs.emacs}/bin/emacsclient -c -a "" $@
          '';
      in
      "${em-path}/bin/em";
  };
  home.packages = with pkgs; [
    # So emacs can compliment me
    espeak
  ];
}
