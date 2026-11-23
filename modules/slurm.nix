# One Slurm module for the whole cluster: the controller (noobnoob) and the
# nodes (win2, win3, later deck).
#
# Before this file, `/etc/nixos/slurm.nix`, `/etc/nixos-wsl/configuration.nix` on
# win2 and the same file on win3 each carried their own copy of the addresses,
# the ids, the node lines and the bootstrap logic, and they drifted: the three
# slurm.conf copies had three different hashes, and the node's `slurm-bootstrap`
# unit only re-fetched when someone restarted it by hand.
#
# The contract here is:
#
#   * the *host descriptions* live in modules/slurm/inventory.nix (one list);
#   * the *constants* live in modules/slurm/constants.nix (one table);
#   * this module turns a host description into a role's NixOS configuration;
#   * the controller fragment (modules/slurm/controller.nix) renders every
#     NodeName=/PartitionName= line from the same inventory;
#   * a node's host configuration (NixOS-WSL, users, sshd, tailnet, toolchain)
#     lives in modules/slurm/node-common.nix, which imports this file.
#
# Refreshing the controller's slurm.conf: the controller's `SLURM_CONF` is a
# /nix/store path that changes on every controller rebuild, and the nodes have
# their own store, so neither a path-typed dependency nor a hash pinned into the
# node's closure can be maintained without a second copy of the controller's
# configuration (the exact drift this module removes). Instead the fetcher, whose
# own store path *is* part of the node's closure, runs before munged/slurmd on
# every boot, resolves the controller's current `SLURM_CONF` with `scontrol`,
# and compares it (path and bytes) with the cached copy. A node rebuild changes
# the fetcher's store path, so systemd re-runs it; a *controller-only* rebuild is
# noticed by the `slurm-config-sync.timer`, which reloads a running slurmd with
# SIGHUP (no job is killed). See `slurm-fetch` below.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.slurmCluster;
  constants = import ./slurm/constants.nix;
  inventory = import ./slurm/inventory.nix;
  render = import ./slurm/render.nix { inherit lib inventory; };

  # The host's entry in the single inventory. `name` is always set by a host
  # file (hosts/win2.nix, …); the throw is for a typo, not a supported state.
  entry =
    inventory.byName.${cfg.name}
      or (throw "slurmCluster.name = \"${cfg.name}\" is not in modules/slurm/inventory.nix");

  isController = cfg.role == "controller";
  isNode = cfg.role == "node";

  # The node's sshfs mount is optional; when it is on, slurmd is ordered after
  # it and refuses to start without it (a job's --chdir would silently fall
  # back to /tmp otherwise).
  workspaceUnit = "slurm-workspace.service";
  workspaceDeps = lib.optional cfg.workspaceMount.enable workspaceUnit;

  # Runs on the node before munged/slurmd, and again from the timer.
  #
  # `--apply` is what the timer passes: only then may the script touch running
  # services. The boot run writes the files and lets systemd's ordering start
  # munged/slurmd with them.
  slurmFetch = pkgs.writeShellScript "slurm-fetch" ''
    set -eu

    key=${lib.escapeShellArg constants.bootstrapKey}
    host=${lib.escapeShellArg "${constants.bootstrapUser}@${cfg.controllerAddr}"}
    conf=${lib.escapeShellArg constants.paths.slurmConf}
    cgroup=${lib.escapeShellArg constants.paths.cgroupConf}
    keyfile=${lib.escapeShellArg constants.paths.mungeKey}
    marker=/run/slurm-bootstrap/conf-path
    apply=0

    if [ "''${1:-}" = "--apply" ]; then
      apply=1
    fi

    install -d -m 0755 /etc/slurm /etc/munge /run/slurm-bootstrap
    # StrictHostKeyChecking=accept-new wants somewhere to write the host key.
    install -d -m 0700 /root/.ssh

    ssh_opts=(
      -i "$key"
      -o BatchMode=yes
      -o StrictHostKeyChecking=accept-new
      -o ConnectTimeout=8
      -o ServerAliveInterval=5
      -o ServerAliveCountMax=2
    )

    # A reachable controller is required for the first run only. If it is not
    # reachable (early boot, tailnet still coming up), keep the cached config
    # and key: both were fetched by an earlier run, and slurmd starting with
    # them beats not starting. A brand-new node has neither, and failing loudly
    # is better than starting munged with a key of its own.
    reachable=0
    if [ -r "$key" ] && timeout 30 ssh "''${ssh_opts[@]}" "$host" true; then
      reachable=1
    fi

    if [ "$reachable" = 0 ]; then
      if [ -s "$conf" ] && [ -s "$keyfile" ]; then
        echo "slurm-fetch: controller unreachable; keeping the cached slurm.conf and munge key" >&2
        exit 0
      fi
      echo "slurm-fetch: controller unreachable and no cached config (is ${constants.bootstrapKey} present?)" >&2
      exit 1
    fi

    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT

    conf_changed=0
    key_changed=0
    cgroup_changed=0

    # Ask the controller where its SLURM_CONF is *right now*. The path changes
    # on every controller rebuild, so it is resolved, never remembered.
    remote_conf=$(timeout 30 ssh "''${ssh_opts[@]}" "$host" \
      "/run/current-system/sw/bin/scontrol show config 2>/dev/null | sed -n 's/^SLURM_CONF *= *//p'") || remote_conf=""

    if [ -n "''${remote_conf:-}" ] \
      && timeout 30 scp -q "''${ssh_opts[@]}" "$host:$remote_conf" "$tmp/slurm.conf" \
      && [ -s "$tmp/slurm.conf" ]; then
      old_path=""
      if [ -r "$marker" ]; then
        old_path=$(cat "$marker")
      fi
      if [ ! -s "$conf" ] || ! cmp -s "$tmp/slurm.conf" "$conf" || [ "$old_path" != "$remote_conf" ]; then
        install -m 0644 -o root -g root "$tmp/slurm.conf" "$conf"
        printf '%s\n' "$remote_conf" > "$marker"
        conf_changed=1
        echo "slurm-fetch: installed slurm.conf from $remote_conf"
      else
        echo "slurm-fetch: slurm.conf unchanged ($remote_conf)"
      fi
    else
      echo "slurm-fetch: could not refresh slurm.conf; keeping the cached one" >&2
    fi

    # cgroup.conf is rendered beside slurm.conf by the controller's Slurm module
    # (ConstrainCores=yes, ConstrainRAMSpace=yes). A node that starts slurmd
    # without it gets Slurm's defaults, where `--mem` and `-c` are only advisory:
    # a job then oversubscribes the node and a later spawn fails with ENOMEM
    # (measured on win2/win3, 2026-11-22 — both had no /etc/slurm/cgroup.conf and
    # `srun -c 4` still saw all 20 CPUs). The task plugin reads this file when it
    # initialises, so a SIGHUP is not enough: a live change waits for the node to
    # be idle, and the marker carries it across timer runs.
    if [ -n "''${remote_conf:-}" ] \
      && timeout 30 scp -q "''${ssh_opts[@]}" "$host:''${remote_conf%/*}/cgroup.conf" "$tmp/cgroup.conf" \
      && [ -s "$tmp/cgroup.conf" ]; then
      if [ -s "$cgroup" ] && cmp -s "$tmp/cgroup.conf" "$cgroup"; then
        echo "slurm-fetch: cgroup.conf unchanged"
      else
        install -m 0644 -o root -g root "$tmp/cgroup.conf" "$cgroup"
        cgroup_changed=1
        if [ "$apply" = 1 ]; then
          touch /run/slurm-bootstrap/cgroup-changed
        fi
        echo "slurm-fetch: installed cgroup.conf from $remote_conf"
      fi
    else
      echo "slurm-fetch: could not refresh cgroup.conf; slurmd runs unconstrained" >&2
    fi

    # The munge key is 0400 munge:munge on the controller, so it is read with
    # sudo and moved base64-encoded (it is random bytes, not text). A truncated
    # key makes munged start and reject every credential, which is worse than a
    # stale key, so only a complete, non-empty decode replaces the file.
    if timeout 30 ssh "''${ssh_opts[@]}" "$host" "sudo -n base64 -w0 /etc/munge/munge.key" \
         > "$tmp/munge.key.b64" 2>/dev/null \
       && [ -s "$tmp/munge.key.b64" ] \
       && base64 -d < "$tmp/munge.key.b64" > "$tmp/munge.key" 2>/dev/null \
       && [ -s "$tmp/munge.key" ]; then
      if cmp -s "$tmp/munge.key" "$keyfile"; then
        echo "slurm-fetch: munge key unchanged"
      else
        install -m 0400 -o munge -g munge "$tmp/munge.key" "$keyfile"
        key_changed=1
        echo "slurm-fetch: installed the controller's munge key"
      fi
    else
      echo "slurm-fetch: could not refresh the munge key; keeping the cached one" >&2
    fi

    # Only the timer's run may disturb running services. A config change is a
    # SIGHUP (slurmd re-reads slurm.conf without killing steps); a key change is
    # the one case that needs a munged restart.
    if [ "$apply" = 1 ]; then
      if [ "$key_changed" = 1 ]; then
        systemctl try-restart munged.service || true
        systemctl try-restart slurmd.service || true
        rm -f /run/slurm-bootstrap/cgroup-changed
      elif [ "$conf_changed" = 1 ]; then
        systemctl try-reload-or-restart slurmd.service || true
      fi
      # A cgroup.conf change needs a slurmd *restart* — the task plugin reads the
      # file at init — and a restart kills the node's steps, so wait until none
      # run. The marker set when the file changed keeps this pending.
      if [ -e /run/slurm-bootstrap/cgroup-changed ]; then
        if ! command -v squeue >/dev/null 2>&1; then
          echo "slurm-fetch: cgroup.conf changed but squeue is missing; not restarting slurmd" >&2
        elif [ -n "$(squeue -h -w "$(hostname -s)" -t RUNNING 2>/dev/null)" ]; then
          echo "slurm-fetch: cgroup.conf changed; waiting for the node to be idle before restarting slurmd" >&2
        else
          systemctl try-restart slurmd.service || true
          rm -f /run/slurm-bootstrap/cgroup-changed
          echo "slurm-fetch: restarted slurmd for the new cgroup.conf"
        fi
      fi
    fi
  '';

  # The controller can generate the cluster key; a node must never do so, or it
  # would authenticate with a key the controller does not have. A node's keygen
  # is therefore a check that the fetcher really installed one.
  mungeKeyScript =
    if isController then
      ''
        set -eu
        if [ ! -s /etc/munge/munge.key ]; then
          install -d -m 0755 -o munge -g munge /etc/munge
          dd if=/dev/urandom of=/etc/munge/munge.key bs=1024 count=1 status=none
          chown munge:munge /etc/munge/munge.key
          chmod 0400 /etc/munge/munge.key
        fi
      ''
    else
      ''
        set -eu
        if [ ! -s /etc/munge/munge.key ]; then
          echo "munge-keygen: no munge key was fetched; refusing to generate one (a node must use the controller's key)" >&2
          exit 1
        fi
        chown munge:munge /etc/munge/munge.key
        chmod 0400 /etc/munge/munge.key
      '';
in
{
  options.slurmCluster = {
    enable = lib.mkEnableOption "this host's membership in the wiz Slurm cluster" // {
      default = true;
    };

    role = lib.mkOption {
      type = lib.types.enum [
        "controller"
        "node"
      ];
      description = ''
        `controller` runs slurmctld and owns the generated slurm.conf;
        `node` runs slurmd and fetches the controller's slurm.conf and munge key.
      '';
    };

    name = lib.mkOption {
      type = lib.types.str;
      description = ''
        This host's Slurm node name. It is also its key in
        `modules/slurm/inventory.nix`, which is where its CPUs, memory,
        features and partitions come from unless overridden here.
      '';
    };

    controllerHost = lib.mkOption {
      type = lib.types.str;
      default = constants.controllerHost;
      defaultText = lib.literalExpression "constants.controllerHost";
      description = "Short hostname of the controller.";
    };

    controllerAddr = lib.mkOption {
      type = lib.types.str;
      default = constants.controllerAddr;
      defaultText = lib.literalExpression "constants.controllerAddr";
      description = "Tailnet address nodes use to reach the controller.";
    };

    cpus = lib.mkOption {
      type = lib.types.ints.positive;
      default = entry.cpus;
      defaultText = lib.literalExpression "inventory.${cfg.name}.cpus";
      description = "CPUs Slurm may allocate on this node.";
    };

    realMemoryMb = lib.mkOption {
      type = lib.types.ints.positive;
      default = entry.realMemoryMb;
      defaultText = lib.literalExpression "inventory.${cfg.name}.realMemoryMb";
      description = "RealMemory= advertised to the controller, in MiB.";
    };

    features = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = entry.features;
      defaultText = lib.literalExpression "inventory.${cfg.name}.features";
      description = "Slurm Features= for `--constraint=...`.";
    };

    partition = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = entry.partition;
      defaultText = lib.literalExpression "inventory.${cfg.name}.partition";
      description = ''
        Partitions this node belongs to. The controller's PartitionName= lines
        are derived from these lists, so this is the only place membership is
        written.
      '';
    };

    state = lib.mkOption {
      type = lib.types.enum [
        "UP"
        "DOWN"
        "DRAIN"
        "UNKNOWN"
        "FUTURE"
      ];
      default = entry.state;
      defaultText = lib.literalExpression "inventory.${cfg.name}.state";
      description = "The boot-time State= the controller records for this node.";
    };

    weight = lib.mkOption {
      type = lib.types.nullOr lib.types.ints.unsigned;
      default = entry.weight;
      defaultText = lib.literalExpression "inventory.${cfg.name}.weight";
      description = "Optional Slurm Weight= (placement bias).";
    };

    nodeAddr = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = entry.addr;
      defaultText = lib.literalExpression "inventory.${cfg.name}.addr";
      description = "The address the controller dials; null for the controller itself.";
    };

    nodePort = lib.mkOption {
      type = lib.types.nullOr lib.types.port;
      default = entry.port;
      defaultText = lib.literalExpression "inventory.${cfg.name}.port";
      description = "Optional Port= for this node (6818 on the WSL nodes).";
    };

    nodeHostname = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = entry.nodeHostname;
      defaultText = lib.literalExpression "inventory.${cfg.name}.nodeHostname";
      description = ''
        Optional NodeHostname=, for a node whose `hostname -s` differs from its
        Slurm name (the Deck is `steamdeck`).
      '';
    };

    topology = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.submodule {
          options = {
            sockets = lib.mkOption { type = lib.types.ints.positive; };
            coresPerSocket = lib.mkOption { type = lib.types.ints.positive; };
            threadsPerCore = lib.mkOption { type = lib.types.ints.positive; };
          };
        }
      );
      default = entry.topology;
      defaultText = lib.literalExpression "inventory.${cfg.name}.topology";
      description = "Optional Sockets=/CoresPerSocket=/ThreadsPerCore= advertisement.";
    };

    workspaceMount = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = entry.workspace;
        defaultText = lib.literalExpression "inventory.${cfg.name}.workspace";
        description = ''
          Mount the controller's /workspace over sshfs with a supervised
          service. Off for the Deck, whose host / is read-only.
        '';
      };
      mountPoint = lib.mkOption {
        type = lib.types.str;
        default = "/workspace";
        description = "Where the mount appears (a job's --chdir expects /workspace).";
      };
      remotePath = lib.mkOption {
        type = lib.types.str;
        default = "/workspace";
        description = "Path exported by the controller.";
      };
      uid = lib.mkOption {
        type = lib.types.ints.unsigned;
        default = constants.uids.reuben;
        description = "Local uid that owns the mounted files.";
      };
      gid = lib.mkOption {
        type = lib.types.ints.unsigned;
        default = constants.gids.users;
        description = "Local gid that owns the mounted files.";
      };
    };

    bootstrapKey = lib.mkOption {
      type = lib.types.str;
      default = constants.bootstrapKey;
      defaultText = lib.literalExpression "constants.bootstrapKey";
      description = ''
        Private key (on the node, provisioned by hand) used to reach the
        controller as `bootstrapUser` for slurm.conf, the munge key and the
        /workspace sshfs.
      '';
    };

    syncInterval = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "10min";
      description = ''
        How often a node re-checks the controller's slurm.conf path and the
        munge key. `null` disables the timer (the check still runs at boot and
        whenever a node rebuild restarts the fetcher).
      '';
    };

    extraPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = "Job toolchain packages for a node (compilers, Lean, ...).";
    };

    nodeEntry = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = render.renderNode {
        name = cfg.name;
        addr = cfg.nodeAddr;
        port = cfg.nodePort;
        inherit (cfg)
          nodeHostname
          cpus
          topology
          realMemoryMb
          features
          weight
          state
          ;
      };
      description = ''
        The NodeName= line this host contributes to the cluster's slurm.conf,
        rendered from the effective options above. Read-only; it exists so
        `nix eval` can compare a node's line with the controller's file.
      '';
    };

    partitionEntries = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      readOnly = true;
      default = map render.renderPartition (
        builtins.filter (p: builtins.elem cfg.name (render.partitionMemberNames p)) inventory.partitions
      );
      description = "The PartitionName= lines this host is a member of.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      assertions = [
        {
          assertion = config.users.groups.slurm.gid == constants.gids.slurm;
          message = "the slurm gid must stay ${toString constants.gids.slurm} (constants.nix)";
        }
        {
          assertion = cfg.partition != [ ];
          message = "slurmCluster.partition must name at least one partition";
        }
      ];

      # munge is the cluster's authentication: the key is the same on every
      # host, 0400 munge:munge, and munged must be up before slurmd.
      services.munge = {
        enable = true;
        password = constants.paths.mungeKey;
      };
      users.users.munge.uid = constants.uids.munge;
      users.groups.munge.gid = constants.gids.munge;
      # uid 307 on every host, so files a job writes agree across the cluster.
      # On the controller services.slurm declares the same uid (ids.uids.slurm);
      # on a node the node block adds isSystemUser.
      users.users.slurm = {
        uid = constants.uids.slurm;
        group = "slurm";
      };
      users.groups.slurm.gid = constants.gids.slurm;

      systemd.services.munge-keygen = {
        description = "ensure the munge key exists with the cluster's ownership";
        wantedBy = [ "munged.service" ];
        before = [ "munged.service" ];
        after = [ "systemd-sysusers.service" ] ++ lib.optional isNode "slurm-bootstrap.service";
        requires = lib.optional isNode "slurm-bootstrap.service";
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = mungeKeyScript;
      };

      # The NixOS slurm module orders slurmd after `munge.service`, which does
      # not exist (the unit is `munged.service`); on a cold boot slurmd could
      # lose the race and exit 1. Require the real unit instead; the restart
      # policy below is the second line of defence.
      systemd.services.slurmd = {
        after = [ "munged.service" ];
        requires = [ "munged.service" ];
        serviceConfig = {
          Restart = "on-failure";
          RestartSec = "5s";
        };
      };

      systemd.tmpfiles.rules = [
        "d /var/spool/slurmd 0755 root root -"
        "d /var/log/slurm 0755 slurm slurm -"
        "d /var/lib/slurm 0755 slurm slurm -"
        "d /run/munge 0755 munge munge -"
      ];

      # The NixOS-WSL image imports / as 0705 and munged refuses to start
      # ("Socket is inaccessible: execute permissions for all required on /").
      # On the controller / is already 0755, so this is a no-op there.
      systemd.services.fix-root-mode = {
        description = "ensure / is 0755 (the NixOS-WSL image ships 0705)";
        wantedBy = [ "basic.target" ];
        before = [ "munged.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = "chmod 0755 /";
      };

      # Slurm's ports on the tailnet only: the controller reaches a node on
      # 6818 and a node reaches the controller on 6817, and neither is exposed
      # on the LAN. (On noobnoob the ssh port 22 is added by configuration.nix.)
      networking.firewall.interfaces.tailscale0.allowedTCPPorts = [
        constants.ports.slurmctld
        constants.ports.slurmd
      ];
      services.tailscale.enable = lib.mkDefault true;
    }

    (lib.mkIf isController {
      # The controller writes the cluster's config itself; nothing to fetch.
      systemd.services.slurmctld.serviceConfig = {
        Restart = "on-failure";
        RestartSec = "5s";
      };
    })

    (lib.mkIf isNode {
      # The node's own name is the Slurm name; NodeHostname is only overridden
      # when the platform's hostname differs (deck/steamdeck).
      networking.hostName = lib.mkDefault cfg.name;

      # The `reuben` account, sshd, the tailnet and the job toolchain are the
      # host's, in modules/slurm/node-common.nix. This module owns only what
      # slurmd itself needs: the slurm user and the binaries.
      users.users.slurm.isSystemUser = true;

      environment.systemPackages =
        [
          pkgs.slurm
          pkgs.munge
          pkgs.openssh
        ]
        ++ lib.optional cfg.workspaceMount.enable pkgs.sshfs
        ++ cfg.extraPackages;
      # The node's config is the controller's file, not a generated one; every
      # slurm command on the node must agree on where it is.
      environment.variables.SLURM_CONF = constants.paths.slurmConf;

      systemd.tmpfiles.rules = lib.optional cfg.workspaceMount.enable
        "d ${cfg.workspaceMount.mountPoint} 0755 root root -";

      systemd.services.slurmd = {
        description = "Slurm node daemon";
        wantedBy = [ "multi-user.target" ];
        wants = [ "network-online.target" ];
        after = [
          "network-online.target"
          "slurm-bootstrap.service"
        ] ++ workspaceDeps;
        requires = [
          "slurm-bootstrap.service"
        ] ++ workspaceDeps;
        serviceConfig = {
          ExecStart = "${pkgs.slurm}/bin/slurmd -D -N ${cfg.name}";
          ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
          Environment = [ "SLURM_CONF=${constants.paths.slurmConf}" ];
          # The cgroup plugins need the step's cgroup visible and writable.
          KillMode = "process";
          Delegate = "Yes";
          LimitMEMLOCK = "infinity";
        };
      };

      # The fetcher *is* part of the node's closure: its store path changes when
      # this module changes, and systemd re-runs it on a rebuild. On every boot
      # it runs before munged and slurmd, so the cached config is refreshed
      # before either daemon reads it.
      systemd.services.slurm-bootstrap = {
        description = "Fetch the controller's slurm.conf and munge key";
        wantedBy = [ "multi-user.target" ];
        before = [
          "munged.service"
          "munge-keygen.service"
          "slurmd.service"
        ];
        after = [
          "network-online.target"
          "tailscaled.service"
        ];
        wants = [
          "network-online.target"
          "tailscaled.service"
        ];
        path = [
          pkgs.openssh
          pkgs.coreutils
          pkgs.diffutils
          pkgs.gnused
          pkgs.systemd
        ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          # Never hold the boot hostage: a hung controller costs this unit.
          TimeoutStartSec = 180;
        };
        script = "${slurmFetch}";
      };

      # A controller-only rebuild does not change a node's closure, so the boot
      # run above cannot see it. This timer re-resolves the controller's
      # SLURM_CONF path and reloads slurmd (SIGHUP) only when it changed.
      systemd.services.slurm-config-sync = lib.mkIf (cfg.syncInterval != null) {
        description = "Re-check the controller's slurm.conf and munge key";
        after = [
          "network-online.target"
          "tailscaled.service"
        ];
        wants = [
          "network-online.target"
          "tailscaled.service"
        ];
        path = [
          pkgs.openssh
          pkgs.coreutils
          pkgs.diffutils
          pkgs.gnused
          pkgs.systemd
        ];
        serviceConfig = {
          Type = "oneshot";
          TimeoutStartSec = 180;
        };
        script = "${slurmFetch} --apply";
      };

      systemd.timers.slurm-config-sync = lib.mkIf (cfg.syncInterval != null) {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "5min";
          OnUnitActiveSec = cfg.syncInterval;
          Unit = "slurm-config-sync.service";
        };
      };

      # A service, not a systemd .mount: FUSE over ssh dies when WSL's network
      # hiccups, and a .mount unit cannot restart itself (which took slurmd
      # down with it). Restart=always re-establishes it; slurmd depends on the
      # service.
      systemd.services.slurm-workspace = lib.mkIf cfg.workspaceMount.enable {
        description = "sshfs mount of the controller's ${cfg.workspaceMount.remotePath}";
        wantedBy = [ "multi-user.target" ];
        wants = [
          "network-online.target"
          "tailscaled.service"
        ];
        after = [
          "network-online.target"
          "tailscaled.service"
        ];
        before = [ "slurmd.service" ];
        path = [
          pkgs.openssh
          pkgs.fuse3
        ];
        serviceConfig = {
          Type = "simple";
          # Best effort: clear a dead endpoint left by the previous attempt.
          ExecStartPre = "-${pkgs.fuse3}/bin/fusermount3 -u ${cfg.workspaceMount.mountPoint}";
          ExecStart = lib.concatStringsSep " " [
            "${pkgs.sshfs}/bin/sshfs"
            "-f"
            "${constants.bootstrapUser}@${cfg.controllerAddr}:${cfg.workspaceMount.remotePath}"
            cfg.workspaceMount.mountPoint
            "-o"
            "IdentityFile=${cfg.bootstrapKey},StrictHostKeyChecking=accept-new"
            "-o"
            "allow_other,uid=${toString cfg.workspaceMount.uid},gid=${toString cfg.workspaceMount.gid},reconnect"
            "-o"
            "ServerAliveInterval=15,ServerAliveCountMax=3"
            "-o"
            "ConnectTimeout=10"
            "-o"
            "dir_cache=yes"
          ];
          Restart = "always";
          RestartSec = "5s";
        };
      };

      programs.fuse = lib.mkIf cfg.workspaceMount.enable {
        enable = true;
        userAllowOther = true;
      };
    })
  ]);
}
