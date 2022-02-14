{ config, lib, pkgs, inputs, ... }:

let
  configPath = ".emacs.d/lisp";
  cleanConfig = ''
  (setq doom-font (font-spec :family "${config.fontMono}" :size 16))
  '';
  cleanConfigName = "config.clean.el";
  dirtyConfigName= "config.dirty.el";
in
{
  home.sessionVariables = {
    EDITOR = "em";
  };

  home.file = {
    "${configPath}/${cleanConfigName}".text = cleanConfig;
    "${configPath}/${dirtyConfigName}".source = config.lib.file.mkOutOfStoreSymlink "${import ./path-to-this-directory.nix}/${dirtyConfigName}";

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
      doomPrivateDir = ./doom;
      extraConfig = ''
      (add-to-list 'load-path "${config.home.homeDirectory}/${configPath}")
      (load "${cleanConfigName}")
      (load "${dirtyConfigName}")
      '';
      extraPackages = [
        epkgs.tsc
        pkgs.cargo
      ];
      emacsPackage = emacs;
      emacsPackagesOverlay = eself: esuper: {
        irony = esuper.irony.overrideAttrs (_: { doCheck = false; });
        sln-mode = esuper.trivialBuild rec {
          pname="sln-mode";
          version="0f91d1b957c7d2a7bab9278ec57b54d57f1dbd9c";
          src = pkgs.fetchFromGitHub {
            owner = "sensorflo";
            repo = "sln-mode";
            rev = version;
            sha256 = "XqkqPyEJuTtFslOz1fpTf/Klbd/zA7IGpzpmum/MGao=";
          };
        };
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
