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

    # https://github.com/Mic92/dotfiles/commit/87d5c4af6ffd190daa92eea153fee77c7cdeb661
    ".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
      mkdir -p $out/bin
      ${lib.concatStringsSep "\n"
        (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") pkgs.tree-sitter.builtGrammars)};
    '');
  };

  # A marriage made in hell
  programs.doom-emacs =
    let
      emacs = pkgs.emacsGit;
      epkgs = pkgs.emacsPackagesFor emacs;
    in {
      enable = true;
      doomPrivateDir = ../doom;
      extraPackages = [ epkgs.lean4-mode epkgs.tsc ];
      emacsPackage = emacs;
    };

  home.packages = with pkgs; [
    # So emacs can compliment me
    espeak
    ripgrep
    tree-sitter

    nixpkgs-fmt
  ];
}
