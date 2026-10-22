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

  # GitHub CLI; also git's credential helper for github.com.
  programs.gh = {
    enable = true;
    settings.git_protocol = "ssh";
  };
}
