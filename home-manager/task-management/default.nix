{ config, lib, pkgs, ... }:

{
  programs.taskwarrior = {
    enable = true;
    package = pkgs.taskwarrior3;
    # config = {
    #   report.descr.columns = "description,status,start.active";
    #   report.descr.labels = "description,status,active";
    # };
  };

  home.packages = [
    pkgs.timewarrior
    pkgs.obsidian
  ];

  home.file = {
    ".local/share/task/hooks/on-modify.timewarrior" = {
      source = "${pkgs.timewarrior}/share/doc/timew/ext/on-modify.timewarrior";
      executable = true;
    };
  };
}
