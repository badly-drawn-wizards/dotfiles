{ config, lib, pkgs, ... }:

{
  programs.obsidian.enable = true;
  home.packages = with pkgs; [
    taskwarrior3
    timewarrior
    # note: the on-modify.timewarrior hook is #!/usr/bin/env python3; python3 is
    # already provided by another home-manager module (adding it here collides).
  ];

  # Timewarrior integration: on `task start` / `task stop`, the shipped hook opens
  # / closes a Timewarrior interval tagged with the task's project + tags, giving
  # `timew summary :week <project>` per-project/day billable totals. Linked from
  # the timewarrior package so it stays valid across rebuilds.
  home.file.".task/hooks/on-modify.timewarrior" = {
    source = "${pkgs.timewarrior}/share/doc/timew/ext/on-modify.timewarrior";
    executable = true;
  };
}
