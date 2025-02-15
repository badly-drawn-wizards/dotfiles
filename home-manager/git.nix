{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    git
    git-lfs
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
      IdentitiesOnly yes
      IdentityFile ~/.ssh/id_ed25519
      IdentityFile ~/.ssh/id_ed25519_whirly

    '';
  };

}
