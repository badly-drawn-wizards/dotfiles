{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "Reuben Steenekamp";
    userEmail = "reuben.steenekamp@gmail.com";
    extraConfig = {
      init.defaultBranch = "master";
      push.default = "simple";
      rebase = {
        autostash = true;
        autosquash = true;
      };
    };
  };
}
