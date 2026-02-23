{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user = {
        name = "Reuben Steenekamp";
        email = "reuben.steenekamp@gmail.com";
      };
      init.defaultBranch = "master";
      push.default = "simple";
      rebase = {
        autostash = true;
        autosquash = true;
      };
    };
  };
}
