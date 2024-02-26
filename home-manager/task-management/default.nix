{ config, lib, pkgs, ... }:

{
  programs.taskwarrior = {
    enable = true;
    config = {
      report.descr.columns = "description,status,start.active";
      report.descr.labels  = "description,status,active";
    };
  };

  home.packages = [
    pkgs.timewarrior
  ];

  home.file = {
    ".local/share/task/hooks/on-modify.timewarrior" = {
      source = "${pkgs.timewarrior}/share/doc/timew/ext/on-modify.timewarrior";
      executable = true;
    };
  };
}
