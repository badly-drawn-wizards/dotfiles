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
      IdentitiesOnly yes
      Host github.com
        User git
        HostName github.com
        PreferredAuthentications publickey
        IdentityFile ~/.ssh/id_ed25519

      Host *.whirlylabs.com
        IdentityFile ~/.ssh/id_ed25519_whirly

    '';
  };

}
