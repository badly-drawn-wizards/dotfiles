{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
      vimHugeX
  ];
  home.file = {
      ".vimrc".text = ''
        set tabstop=2
        set shiftwidth=2
        set expandtab
        set autochdir
        set tags=./tags,tags
      '';
  };
}
