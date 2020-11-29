{ config, lib, pkgs, ... }:

{
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

  home.packages = with pkgs; [
    # So emacs can compliment me
    espeak
  ];
}
