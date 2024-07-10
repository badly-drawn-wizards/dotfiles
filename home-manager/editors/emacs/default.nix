{ config, lib, pkgs, inputs, ... }:

let
  inherit (pkgs) writeScriptBin;
  inherit (config.programs.doom-emacs.extraSiteLisp) site-lisp;

  configName = "config.el";

  em = writeScriptBin "em" ''
    #!/usr/bin/env sh
    emacsclient -c -a "emacs" $@
  '';

in
{
  imports = [
    ./extra-site-lisp.nix
    ./status-helpers.nix
  ];

  # A marriage made in hell
  programs.doom-emacs =
    let
      inherit (pkgs) emacs;
    in
    {
      enable = true;
      doomDir = ./doom;
      # extraConfig = ''
      #   (add-to-list 'load-path "${site-lisp}")
      #   (setq doom-font (font-spec :family "${config.fontMono}" :size 16))
      #   (load "${configName}")
      # '';
      # emacsPackagesOverlay = eself: esuper: {
      #   inherit (pkgs.lean4) lean4-mode;
      #   # inherit (oepkgs) evil-collection;
      # };
    };

  programs.zsh.initExtraDag.vterm-init = lib.hm.dag.entryAfter [ "bind-keys" ] ''
    if [[ $INSIDE_EMACS = vterm ]]; then
      bindkey "^J" accept-line
      if [[ -n ''${EMACS_VTERM_PATH} ]] \
        && [[ -f ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
        source ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
      fi
    fi
  '';

  home.packages = with pkgs; [
    em
  ];
}
