{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    git
  ];
  home.file = {
    ".gitconfig".text = ''
      [user]
        email = reuben.steenekamp@gmail.com
        name = Reuben Steenekamp
      [push]
        default = simple
      [rebase]
        autostash = true
        autosquash = true
    '';
    ".ssh/config".text = ''
      Host github
        User git
        HostName github.com

      IdentityFile ~/.ssh/id_rsa
    '';
  };

}
