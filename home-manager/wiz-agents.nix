# The agent/desktop systemd *user* units as NixOS/home-manager declarations.
#
# These were hand-written in ~/.config/systemd/user and drifted: they survived
# edits, were invisible to `git -C /etc/nixos log`, and were not reproducible on a
# fresh machine. This module is the source of truth; the files under
# ~/.config/systemd/user are now links into the store.
#
# Slices use the first-class `systemd.user.slices` option; nothing here writes a
# raw file into ~/.config.
{ pkgs, ... }:
let
  guard = pkgs.writeShellScript "opencode-serve-guard" ''
    # opencode-serve-guard - idempotent singleton launcher for the OpenCode
    # background server.
    #
    # Why this exists: the TUI spawns `opencode serve --service` on its own when its
    # event stream stalls. Four clients therefore fork four servers in ~350 ms. A
    # losing spawn exits 0 in ~160 ms without touching the winner (verified), but the
    # winner is a child of whichever TUI spawned it and dies with that TUI -- which
    # is how the box ended up with no server for 4 min 40 s.
    #
    # This wrapper makes the port the source of truth: if anything already listens,
    # wait instead of fighting, and take over the moment it releases the port. The
    # systemd unit therefore never kills a live server and never runs two.
    set -u

    PORT="''${OPENCODE_SERVE_PORT:-49374}"
    BIN="''${OPENCODE_BIN:-/etc/profiles/per-user/reuben/bin/opencode}"
    SS="''${OPENCODE_SS:-/run/current-system/sw/bin/ss}"

    if "$SS" -ltnH "sport = :$PORT" | grep -q .; then
      echo "opencode-serve-guard: port $PORT already served; waiting for it to be released" >&2
      while "$SS" -ltnH "sport = :$PORT" | grep -q .; do
        sleep 2
      done
      echo "opencode-serve-guard: port $PORT released; taking over" >&2
    fi

    exec "$BIN" serve --service --port "$PORT"
  '';
in
{
  home.file.".local/bin/opencode-serve-guard".source = guard;

  # --- slices -------------------------------------------------------------------
  systemd.user.slices.desktop = {
    Unit.Description = "Interactive desktop apps (protected from build reclaim)";
    Slice = {
      # memory.min/low are *protections*: reclaim prefers the build slice first.
      MemoryMin = "2G";
      MemoryLow = "3G";
      MemoryHigh = "5G";
      MemoryMax = "8G";
      MemorySwapMax = "6G";
      CPUWeight = 200;
      # Keep systemd-oomd and the kernel away from the UI if a build runs away.
      ManagedOOMPreference = "avoid";
    };
  };

  systemd.user.slices.wiz-agents = {
    Unit.Description = "Agent tool shells (OpenCode spawns; separate from the protected server)";
    Slice = {
      # The server is protected; its children are not. A build that a shell starts
      # lands here, where the cap and the OOM preference are the scheduler's, not
      # the desktop session's.
      MemoryHigh = "3G";
      MemoryMax = "4G";
      CPUWeight = 50;
      IOWeight = 50;
    };
  };

  # --- the server ---------------------------------------------------------------
  systemd.user.services.opencode-serve = {
    Unit = {
      Description = "OpenCode background server (singleton on 127.0.0.1:49374)";
      Documentation = [ "file:/workspace/wiz-notes/opencode-serve.md" ];
      # Never trip the rate limiter: a memory stall may kill the server repeatedly,
      # and we want one restart every RestartSec, not a start-limit-hit.
      StartLimitIntervalSec = 0;
    };
    Service = {
      Type = "simple";
      WorkingDirectory = "%h";
      Environment = [
        "HOME=%h"
        "OPENCODE_DISABLE_AUTOUPDATE=true"
        "OPENCODE_SERVE_PORT=49374"
        "SHELL=%h/.local/bin/agent-shell"
        "PATH=/etc/profiles/per-user/reuben/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin:%h/.local/bin"
      ];
      ExecStart = "%h/.local/bin/opencode-serve-guard";
      # Protect the server from memory pressure; its spawns are moved to
      # wiz-agents.slice by agent-shell, so this is the one process to keep alive.
      MemoryMin = "512M";
      MemoryLow = "1G";
      ManagedOOMPreference = "avoid";
      # An OOM kill is a reason to restart, not to stay down.
      Restart = "always";
      RestartSec = 2;
      # A stalled/crashed server must not take the agent shells with it.
      TimeoutStopSec = 20;
      KillMode = "control-group";
      # Live generation ~730 MB RSS. 2G high throttles reclaim, 4G max bounds a
      # runaway; negative OOMScoreAdjust protects it from the kernel (a positive
      # value made it a preferred victim), and OOMPolicy=continue keeps the unit
      # alive when a *child* is the OOM victim.
      OOMScoreAdjust = -500;
      MemoryHigh = "2G";
      MemoryMax = "4G";
      OOMPolicy = "continue";
      # 512 was hit by the server's own children: the MCP servers (playwright's
      # headless Chromium alone is ~50 tasks) and the per-session runtime all live
      # in this cgroup, so at 510/512 every fork failed with EAGAIN
      # (`failed to spawn terminal reader`), which looks like a spawn regression.
      # pids.events showed `max 100`. Keep the memory caps as the real bound.
      TasksMax = 4096;
      LimitNOFILE = 65536;
    };
    Install.WantedBy = [ "default.target" ];
  };

  # --- watchdog and findings timers ----------------------------------------------
  systemd.user.services.wiz-mem-watch = {
    Unit = {
      Description = "Wiz memory-pressure watchdog (kills only wiz-heavy.slice scopes)";
      Documentation = [ "file:///workspace/wiz-notes/swap-stability.md" ];
      # The watchdog must not be a build: it is tiny, but keep it off the heavy slice.
      After = [ "default.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "/workspace/wiz-tools/wiz-mem-watch";
      Nice = 5;
      IOSchedulingClass = "idle";
      # No cgroup caps: the sampler must run even when the machine is stalling.
      OOMPolicy = "continue";
    };
  };

  systemd.user.timers.wiz-mem-watch = {
    Unit.Description = "Run the Wiz memory-pressure watchdog every minute";
    Timer = {
      OnBootSec = "3min";
      OnUnitActiveSec = "60s";
      AccuracySec = "10s";
      Unit = "wiz-mem-watch.service";
    };
    Install.WantedBy = [ "timers.target" ];
  };

  systemd.user.services.wiz-review-findings = {
    Unit = {
      Description = "Validate the adversarial-review findings and surface open blockers";
      After = [ "network.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "/workspace/wiz-tools/review-findings-cron.sh";
    };
  };

  systemd.user.timers.wiz-review-findings = {
    Unit.Description = "Every 6 h: review findings validation + open-blocker nudge";
    Timer = {
      OnCalendar = "*-*-* 00/6:17:00";
      Persistent = true;
      RandomizedDelaySec = 300;
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
