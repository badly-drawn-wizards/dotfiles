{ config, lib, pkgs, ... }:

let
  inherit (lib.strings) removePrefix;

  # https://github.com/Mic92/dotfiles/commit/87d5c4af6ffd190daa92eea153fee77c7cdeb661
  tree-sitter-grammars =
    let
      stripPre = removePrefix "tree-sitter-";
      symlinkParser = name: src:
        "ln -s ${src}/parser $out/bin/${stripPre name}.so";
      script = lib.concatStringsSep "\n" (
        ["mkdir -p $out/bin"] ++
        lib.mapAttrsToList
          symlinkParser
          pkgs.tree-sitter.builtGrammars
      );
    in
      pkgs.runCommand "grammars" {} script;
in
{
  home.file = {
      ".tree-sitter".source = tree-sitter-grammars;
  };
}
