{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
      # A marriage made in hell
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
      ".ideavimrc".source = ./.ideavimrc;
  };
}
