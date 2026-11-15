# Persist the memory/OOM policy from wiz-notes/swap-stability.md (§5.1, §6.2–6.4, §7.1)
# on this NixOS 26.11pre host, so it survives reboot without a `switch` being run now.
#
# Scope of this module: everything that needs a *system* unit, a kernel sysctl or
# /etc. The two user-level slices (`wiz-heavy.slice`, `desktop.slice`) live in
# ~/.config/systemd/user/ and are already persistent; `heavy` is a file in
# /workspace/wiz-tools. Nothing here duplicates them.
#
# What this module deliberately does NOT do:
#   * it does not touch vm.swappiness, overcommit, the swap devices, or zram size
#     (see the note: those are not the shortage, and changing zram at runtime
#     means swapping ~10 GiB back into RAM);
#   * it does not ship a `session-N.scope.d` drop-in. Scope drop-ins *do* work
#     (verified here: one applies at scope creation and on daemon-reload), but
#     logind assigns the unit name and it is not stable across sessions (a tty or
#     ssh login can take session-1). `wiz-session-protect` below discovers the
#     seat's wayland/x11 session instead and re-applies the properties at every
#     login, so it also survives logout/login and not just reboot.
#
# Apply (from a shell, as the human):
#   sudo nixos-rebuild switch --flake /etc/nixos#noobnoob
# Live-effect summary is in the header of each section.
#
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.wiz.memory;
  uidStr = toString cfg.uid;
in
{
  options.wiz.memory = {
    enable = lib.mkEnableOption "the Wiz memory/OOM policy (swap-stability.md P0/P1)" // {
      # This module exists to be the policy; importing it is the decision.
      # Set `wiz.memory.enable = false` to turn it back off.
      default = true;
    };

    uid = lib.mkOption {
      type = lib.types.int;
      default = 1000;
      description = "UID whose graphical logind session (`session-*.scope`) gets protected.";
    };

    desktop = {
      memoryMin = lib.mkOption {
        type = lib.types.str;
        default = "2G";
        description = "Hard protection for the graphical session scope. Never reclaimed.";
      };
      memoryLow = lib.mkOption {
        type = lib.types.str;
        default = "3G";
        description = "Best-effort protection for the graphical session scope.";
      };
      memorySwapMax = lib.mkOption {
        type = lib.types.str;
        default = "6G";
        description = "Swap ceiling for the graphical session scope (desktop peak was 8.5 GiB).";
      };
    };

    nixDaemon = {
      memoryHigh = lib.mkOption { type = lib.types.str; default = "6G"; };
      memoryMax = lib.mkOption { type = lib.types.str; default = "8G"; };
      memorySwapMax = lib.mkOption { type = lib.types.str; default = "1G"; };
      maxJobs = lib.mkOption { type = lib.types.int; default = 2; };
      cores = lib.mkOption { type = lib.types.int; default = 6; };
    };

    userManager = {
      # user@ holds terminals, OpenCode and (transitively) wiz-heavy.slice.
      # 13G = wiz-heavy's 10G ceiling + ~3G for the agents/terminals that
      # launch the jobs. High below Max so reclaim starts first.
      memoryHigh = lib.mkOption { type = lib.types.str; default = "11G"; };
      memoryMax = lib.mkOption { type = lib.types.str; default = "13G"; };
      memorySwapMax = lib.mkOption { type = lib.types.str; default = "6G"; };
    };

    userSlice = {
      # Outer guard: session-1 (desktop) + user@ (builds/terminals). 17G of
      # 27 GiB leaves ~8G for nix-daemon + system.slice + kernel.
      memoryHigh = lib.mkOption { type = lib.types.str; default = "15G"; };
      memoryMax = lib.mkOption { type = lib.types.str; default = "17G"; };
    };

    oomd = {
      swapUsedLimit = lib.mkOption { type = lib.types.str; default = "50%"; };
      memoryPressureLimit = lib.mkOption { type = lib.types.str; default = "25%"; };
      pressureDuration = lib.mkOption { type = lib.types.str; default = "20s"; };
    };

    zramResidentLimit = lib.mkOption {
      type = lib.types.str;
      default = "6 * 1024 * 1024 * 1024";
      description = ''
        Cap on the RAM the zram device may consume for compressed pages
        (/sys/block/zram0/mem_limit). zram pages are kernel memory and are not
        charged to any cgroup, so an unbounded zram can eat the RAM it saves.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # --- kernel: make kswapd start earlier and keep writeback/reclaim bounded ---
    # All of these apply live on `nixos-rebuild switch` (systemd-sysctl), no reboot.
    boot.kernel.sysctl = {
      # 66 MiB was <1 s of allocation at the observed 60 MB/s. 256 MiB (~1 % of
      # RAM) plus the watermark gap below gives kswapd ~800 MiB of headroom
      # before a allocating task has to enter direct reclaim. Deliberately half
      # of the note's 512M: 512M + scale_factor=150 permanently reserves ~1.3 GiB
      # of a 27 GiB machine, which is build headroom we cannot spare once the
      # cgroup caps are in place. Raise to 524288/150 only if PSI stays high.
      "vm.min_free_kbytes" = 262144;
      # Default 10 = 0.1 %; 100 widens the min→low→high gap to ~270 MiB each, so
      # background reclaim starts before the allocating task stalls.
      "vm.watermark_scale_factor" = 100;
      # Disk swap is NVMe and refaults are random; default 3 reads 32 KiB per
      # major fault. 1 (8 KiB) halves the wasted readahead. zram is unaffected.
      "vm.page-cluster" = 1;
      # Retain Nix-store dentries/inodes across builds instead of dropping them
      # and re-walking the store. Optional / low confidence: drop this first if
      # slab_reclaimable grows.
      "vm.vfs_cache_pressure" = 50;
      # Nix/lake write multi-GB outputs; the default 20 %/10 % allows 5.4/2.7 GiB
      # of dirty pages, and a foreground app can block in balance_dirty_pages.
      # 1 GiB / 256 MiB bounds the stall; builds write back more often instead.
      "vm.dirty_bytes" = 1073741824;
      "vm.dirty_background_bytes" = 268435456;
      # THP is `madvise` here, so aggressive proactive compaction mostly burns
      # kcompactd CPU on a 34 GiB-VSZ workload. 10 is a mild reduction, not off.
      "vm.compaction_proactiveness" = 10;
      # 84 = keyboard(4) + sync(16) + signal/oom-kill(64). Enables Alt-SysRq+f
      # (kill the OOM victim) for the §9.4 emergency path while keeping
      # reboot/poweroff(128) and "kill everything"(i is in 64; f is the safe one)
      # out of casual reach. Not 1 (all keys, including b).
      "kernel.sysrq" = 84;
    };

    # MGLRU: don't evict pages younger than 1 s. Directly attacks the refault
    # churn (96.9 M anon refaults) during a burst. sysfs, so tmpfiles at boot;
    # applies on switch only after the next boot (or a manual write as root).
    systemd.tmpfiles.rules = [
      "w /sys/kernel/mm/lru_gen/min_ttl_ms - - - - 1000"
    ];

    # --- journald: bound it, and restart it ---
    # journald was killed by its own watchdog on 2026-11-13 after reading 32.8 GB and
    # writing 12.2 GB in 2d20h; with no restart it silently stopped logging for two days,
    # so the next incident had no evidence at all.
    # (26.11: `extraConfig` is gone; `settings` are written into `[Journal]` directly.)
    services.journald.settings = lib.mkDefault {
      SystemMaxUse = "2G";
      SystemMaxFileSize = "256M";
      RateLimitIntervalSec = "30s";
      RateLimitBurst = "20000";
    };
    systemd.services.systemd-journald.serviceConfig = {
      Restart = lib.mkDefault "always";
      RestartSec = lib.mkDefault "2s";
    };

    # --- zram: cap the hidden RAM, keep the disk size ---
    # Already enabled with memoryPercent = 50 by hardware/default.nix. The
    # *resident* limit (kernel mem_limit) is what was missing: 11.9 GiB of
    # compressed data used 1.8 GiB of RAM that no cgroup accounts for.
    zramSwap = {
      enable = true;
      memoryPercent = 50;
    };
    services.zram-generator.settings."zram0"."zram-resident-limit" =
      cfg.zramResidentLimit;

    # --- systemd-oomd: actually monitor something ---
    # `kill` is the only value that enables monitoring; the default `auto` is why
    # oomctl listed zero cgroups at the incident. Root/system/user slices stay
    # unmonitored on purpose: we want targeted victims (user@, wiz-heavy), not a
    # whole-tree sweep that can include session-1.
    systemd.oomd = {
      enable = true;
      enableRootSlice = false;
      enableSystemSlice = false;
      enableUserSlices = false;
      settings.OOM = {
        SwapUsedLimit = cfg.oomd.swapUsedLimit;
        DefaultMemoryPressureLimit = cfg.oomd.memoryPressureLimit;
        DefaultMemoryPressureDurationSec = cfg.oomd.pressureDuration;
      };
    };

    # --- cgroup budget for 27 GiB of RAM ---
    # nix-daemon 8G + user.slice 17G + system.slice ~1.5G, with the desktop's 2G
    # min inside user.slice. The hard cap on user-1000 is a backstop only; the
    # useful caps are the two below it.
    #
    # user-1000.slice: the outer guard. `session-1.scope` (desktop) and
    # `user@1000.service` (terminals, OpenCode, builds) are siblings in here.
    # Because session-1 carries memory.min/low, reclaim inside this slice hits
    # the builds first — the inversion in §4.2.
    systemd.slices."user-${uidStr}".sliceConfig = {
      MemoryHigh = cfg.userSlice.memoryHigh;
      MemoryMax = cfg.userSlice.memoryMax;
    };

    # user@1000.service: everything a terminal/agent starts directly. This is
    # the cgroup that held the 34 GiB `wiz check` (via a kitty scope). With a
    # MemoryHigh, the kernel reclaims/throttles there before global pressure
    # reaches the desktop; with MemorySwapMax it cannot accumulate 30 GiB of
    # swap. ManagedOOMSwap=kill gives oomd a targeted victim (the descendant
    # with the most swap): at 02:20 that was the kitty scope with 30.4 GiB.
    # OOMPolicy=continue so an OOM kill of a descendant never stops the whole
    # user manager (and with it every terminal and OpenCode session).
    systemd.services."user@".serviceConfig = {
      MemoryHigh = cfg.userManager.memoryHigh;
      MemoryMax = cfg.userManager.memoryMax;
      MemorySwapMax = cfg.userManager.memorySwapMax;
      ManagedOOMSwap = "kill";
      # Explicit: do not kill terminals on routine PSI. Swap-based selection is
      # the signal that matched this incident.
      ManagedOOMMemoryPressure = "auto";
      OOMPolicy = "continue";
    };

    # nix-daemon: 11G/9G was half the machine for a 4×8 build fleet. 2 jobs × 6
    # cores fits in 8G and leaves room for the desktop. mkForce because
    # hardware/default.nix:14-27 already sets these; delete those lines instead
    # if you prefer one source of truth.
    systemd.services.nix-daemon.serviceConfig = {
      MemoryHigh = lib.mkForce cfg.nixDaemon.memoryHigh;
      MemoryMax = lib.mkForce cfg.nixDaemon.memoryMax;
      MemorySwapMax = lib.mkForce cfg.nixDaemon.memorySwapMax;
    };
    nix.settings = {
      max-jobs = lib.mkForce cfg.nixDaemon.maxJobs;
      cores = lib.mkForce cfg.nixDaemon.cores;
    };

    # --- desktop protection without a session-number-dependent unit ---
    # Applies MemoryMin/Low/SwapMax + ManagedOOMPreference=avoid to the seat's
    # graphical session scope. `set-property --runtime` is used because scopes
    # are transient; the path unit re-runs this on every login, so it also
    # survives logout/login (which creates session-2.scope).
    #
    # No MemoryMax/MemoryHigh on the session scope: a hard cap here would OOM-kill
    # the compositor during a legitimate spike (the observed peak was 16.5 GiB
    # resident). Protection is what the desktop needs; containment belongs on
    # user@ and wiz-heavy.
    systemd.services.wiz-session-protect = {
      description = "Apply memory protection to the graphical logind session scope";
      after = [ "systemd-logind.service" ];
      path = [ config.systemd.package pkgs.coreutils ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
        # The .path watches /run/systemd/sessions, which systemd rewrites on session
        # metadata changes; a burst hit the default start limit and left the unit
        # "failed", so the next login silently lost desktop protection.
        StartLimitIntervalSec = 0;
      };
      script = ''
        set -u
        deadline=$(( $(date +%s) + 10 ))
        while :; do
          applied=0
          for d in /sys/fs/cgroup/user.slice/user-${uidStr}.slice/session-*.scope; do
            [ -d "$d" ] || continue
            unit=''${d##*/}
            sid=''${unit#session-}
            sid=''${sid%.scope}
            case "$sid" in ""|*[!0-9]*) continue ;; esac
            # Only the local, graphical, user session. Never a tty/ssh session:
            # those must stay killable by oomd.
            typ=$(loginctl show-session "$sid" -p Type --value 2>/dev/null) || continue
            remote=$(loginctl show-session "$sid" -p Remote --value 2>/dev/null) || continue
            class=$(loginctl show-session "$sid" -p Class --value 2>/dev/null) || continue
            [ "$remote" = no ] || continue
            [ "$class" = user ] || continue
            case "$typ" in wayland|x11) ;; *) continue ;; esac
            if systemctl set-property --runtime "$unit" \
                 MemoryMin=${cfg.desktop.memoryMin} \
                 MemoryLow=${cfg.desktop.memoryLow} \
                 MemorySwapMax=${cfg.desktop.memorySwapMax} \
                 IOAccounting=yes IOWeight=200 \
                 ManagedOOMPreference=avoid; then
              applied=1
              echo "wiz-session-protect: $unit protected (min ${cfg.desktop.memoryMin}, low ${cfg.desktop.memoryLow})"
            fi
          done
          [ "$applied" = 1 ] && break
          [ "$(date +%s)" -ge "$deadline" ] && break
          sleep 1
        done
      '';
    };

    systemd.paths.wiz-session-protect = {
      description = "Re-apply session memory protection on every logind login";
      wantedBy = [ "paths.target" ];
      pathConfig = {
        PathChanged = "/run/systemd/sessions";
        Unit = "wiz-session-protect.service";
      };
    };
  };
}
