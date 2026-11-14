{
  config,
  pkgs,
  lib,
  ...
}:
let
  # gh needs the user's keyring/DBus session (systemd user units have it); git reads
  # /workspace; the rest are plain POSIX tools.
  checkPath = lib.makeBinPath [
    pkgs.gh
    pkgs.git
    pkgs.coreutils
    pkgs.gawk
  ];
in
{
  home.file.".local/bin/wiz-coordinator-check" = {
    source = ./scripts/wiz-coordinator-check.sh;
    executable = true;
  };

  systemd.user.services.wiz-coordinator-check = {
    Unit.Description = "Wiz coordinator check (read-only)";
    Service = {
      Type = "oneshot";
      WorkingDirectory = "/workspace/wiz-lang";
      Environment = [ "PATH=${checkPath}" ];
      ExecStart = "%h/.local/bin/wiz-coordinator-check";
      Nice = 10;
    };
  };

  systemd.user.timers.wiz-coordinator-check = {
    Unit.Description = "Run the Wiz coordinator check every 30 minutes";
    Timer = {
      OnCalendar = "*:0/30";
      Persistent = true;
      RandomizedDelaySec = "60s";
      AccuracySec = "1min";
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
