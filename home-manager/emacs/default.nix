{ config, lib, pkgs, inputs, ... }:

let
  configPath = ".emacs.d/lisp";
  cleanConfig = ''
  (setq doom-font (font-spec :family "${config.fontMono}" :size 16))
  '';
  cleanConfigName = "config.clean.el";
  dirtyConfigName = "config.dirty.el";
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
      doomPrivateDir = ./doom;
    in {
      enable = true;
      doomPackageDir =
        let
          filteredPath = builtins.path {
            path = doomPrivateDir;
            name = "doom-private-dir-filtered";
            filter = path: type:
              builtins.elem (baseNameOf path) [ "init.el" "packages.el" ];
          };
        in
          pkgs.linkFarm "doom-packages-dir" [
          {
            name = "init.el";
            path = "${filteredPath}/init.el";
          }
          {
            name = "packages.el";
            path = "${filteredPath}/packages.el";
          }
          {
            name = "config.el";
            path = pkgs.emptyFile;
          }];
      inherit doomPrivateDir;
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
        # irony = esuper.irony.overrideAttrs (_: { doCheck = false; });
        lean4-mode = pkgs.lean4.lean4-mode;
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
