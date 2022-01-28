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
      emacs = pkgs.emacs;
      epkgs = pkgs.emacsPackagesFor emacs;
    in {
      enable = true;
      doomPrivateDir = ../doom;
      extraPackages = [ epkgs.lean4-mode epkgs.tsc ];
      emacsPackage = emacs;
      emacsPackagesOverlay = eself: esuper: {
        irony = esuper.irony.overrideAttrs (_: { doCheck = false; });

        lean4-mode = esuper.trivialBuild {
          pname = "lean4-mode";
          version = "v4.0.0-m2";
          src = pkgs.fetchFromGitHub {
            owner = "leanprover";
            repo = "lean4";
            rev = "26dda3f63d885e8c22888926bdea0d99f58bf444";
            sha256 = "e0bDkcyd8PYzU1KuPkgZFgC/bPTC9fuFQzc6mMzL9LY=";
            fetchSubmodules = true;
          };
          sourceRoot = "source/lean4-mode";
          packageRequires = with eself; [ dash dash-functional f flycheck lsp-mode magit-section s ];
        };
      };
    };

  home.packages = with pkgs; [
    # So emacs can compliment me
    espeak
    ripgrep
    tree-sitter

    nixpkgs-fmt
  ];
}
