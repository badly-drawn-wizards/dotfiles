{ config, lib, pkgs, ... }:

let
  inherit (config.programs.doom-emacs.extraSiteLisp) path site-lisp;
in
{
  options = with lib; with types; {
    programs.doom-emacs.extraSiteLisp = {
      path = mkOption {
        type = str;
        default = ".emacs.d/site-lisp";
      };
      site-lisp = mkOption {
        type = package;
        default = config.lib.file.mkOutOfStoreSymlink
          "${import ./path-to-this-directory.nix}/site-lisp";
      };
    };
  };
  config = {
    home.file = {
      "${path}".source = site-lisp;
    };
  };
}
