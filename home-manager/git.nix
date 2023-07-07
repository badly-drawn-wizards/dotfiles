{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    git
  ];
  home.file = {
    ".gitconfig".text = ''
      [init]
        defaultBranch = master
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

      Host *
        IdentityFile ~/.ssh/id_rsa
        IdentityFile ~/.ssh/id_ed25519_whirly
        IdentitiesOnly yes
    '';
  };

}
