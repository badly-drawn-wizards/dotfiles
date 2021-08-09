{ pkgs, ... }:

{
  home.packages = with pkgs; [
    idea-community
  ];

  home.file = {
    ".ideavimrc".source = ./.ideavimrc;
  };
}
