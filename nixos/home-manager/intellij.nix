{ pkgs, ... }:

{
  home.packages = with pkgs; [
    idea
  ];

  home.file = {
    ".ideavimrc".source = ./.ideavimrc;
  };
}
