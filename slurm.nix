# Slurm, single node, for the heavy build queue (2026-11-17).
#
# The migration is deliberately *not* switched on: `heavy` keeps running the queue
# (wiz-tools/heavy), and `wiz-tools/heavy-srun` is the same `heavy --mem N cmd`
# interface on top of `srun`. When the queue needs to survive a reboot or more than
# one user, `heavy` becomes a thin wrapper over that script.
#
# Accounting: Slurm reports MiB. The box has 27 GiB; RealMemory leaves room for the
# OS, the desktop session and nix-daemon (which runs outside any job). `heavy`'s own
# budget is 16 G; this node is sized so a job can be larger than that when the queue
# is quiet (`--mem 20G`), which is the point of having a real scheduler.
{ config, lib, pkgs, ... }:
let
  node = "noobnoob";
  # 27 GiB total; 24 GiB is the largest a job may reserve, so ~3 GiB stays with the OS.
  realMemory = 24000;
in
{
  services.slurm = {
    server.enable = true;  # slurmctld
    client.enable = true;  # slurmd

    controlMachine = node;
    # Remote nodes resolve the controller by tailnet address, not MagicDNS.
    controlAddr = "100.76.41.56";
    clusterName = node;
    # CPUs= is the count Slurm may allocate; 0-23 are for jobs, 24-31 are reserved for
    # the perf window, so advertise 24 rather than `nproc` (32).
    nodeName = [
      # Weight biases placement: noobnoob is preferred, and the WSL2 node takes jobs
      # only when it is full.
      "${node} CPUs=24 RealMemory=${toString realMemory} Weight=100 State=UNKNOWN"
      # The Steam Deck: registered DOWN so the partition exists and a job can be
      # aimed at it, but nothing is scheduled there until its slurmd answers.
      # `scontrol update NodeName=deck State=RESUME` after its slurmd is up.
      # NodeAddr is the Deck's tailnet address; NodeHostname is what `hostname -s`
      # returns there (steamdeck), which is how slurmd finds its own NodeName.
      "deck NodeAddr=100.106.233.3 NodeHostname=steamdeck CPUs=8 RealMemory=14000 Features=steamdeck State=DOWN"
      # `win` (the Docker Desktop container) was retired on 2026-11-18 in favour of
      # `win2`. Its container and image were deleted; `wiz-notes/windows-slurm/` keeps
      # the recipe, so it can be rebuilt if the WSL2 node ever fails.
      # The WSL2 node (NixOS-WSL, systemd), the replacement for the retired container.
      # It shares the controller's *jobs* store only through `nix copy`, so a job that
      # names a store path works after that import.
      "win2 NodeAddr=100.93.9.54 Port=6818 NodeHostname=win2 CPUs=20 Sockets=1 CoresPerSocket=10 ThreadsPerCore=2 RealMemory=15800 Features=wsl2 State=UNKNOWN"
      # The Windows laptop (NixOS-WSL), same shape as win2. It advertises 22000 MiB of a
      # 24 GiB cap: the cap is a ceiling, not a reservation, so leave Windows headroom.
      # It gets its own partition rather than `batch`, because a laptop that sleeps takes
      # its jobs with it.
      "win3 NodeAddr=100.91.219.16 Port=6818 NodeHostname=win3 CPUs=20 Sockets=1 CoresPerSocket=10 ThreadsPerCore=2 RealMemory=22000 Features=wsl2 State=UNKNOWN"
    ];
    partitionName = [
      # win2 is in the pool at the default weight: it has the controller's toolchain and
      # builds (`lake build wiz` with `-c 8 --mem 12G`), but no shared /nix/store, so a
      # job that names a store path from this machine still needs `--nodelist=noobnoob`.
      "batch Nodes=${node},win2,win3 Default=YES MaxTime=INFINITE State=UP"
      "deck  Nodes=deck  MaxTime=04:00:00 State=UP"
      "win2  Nodes=win2  MaxTime=04:00:00 State=UP"
    ];

    # cgroup v2 for tracking and for the memory cap; spank-free.
    procTrackType = "proctrack/cgroup";

    extraConfig = ''
      SelectType=select/cons_tres
      SelectTypeParameters=CR_Core_Memory
      TaskPlugin=task/cgroup
      # A node that rebooted or whose slurmd died comes back by itself.
      ReturnToService=2
      # Slurm sizes the node table at startup; adding a node later needs this raised above
      # the number of NodeName lines (scontrol create fails with "node table is full").
      MaxNodeCount=8
      # A job that does not ask for memory must not be given the rest of the node: with
      # CR_Core_Memory an unbounded request reserved ~10 GiB each and starved the queue.
      DefMemPerCPU=1024
      # A suspended Deck must stop holding a job instead of waiting for a timeout.
      SlurmdTimeout=120
      # `srun` blocks and streams like `heavy`; keep the default Ctrl-C behaviour.
      KillWait=30
    '';

    # ConstrainRAMSpace is the part that makes `--mem` real rather than advisory:
    # the step's cgroup gets its allocation, and going over it is an OOM kill inside
    # the job (the same failure mode heavy already reports as exit 143).
    extraCgroupConfig = ''
      CgroupPlugin=autodetect
      ConstrainCores=yes
      ConstrainRAMSpace=yes
      ConstrainSwapSpace=yes
      AllowedRAMSpace=100
      MaxRAMPercent=100
      AllowedSwapSpace=0
      MaxSwapPercent=0
    '';
  };

  # slurmd/slurmctld can lose the race with munged's socket on a cold boot and exit 1.
  # A retry is cheaper than a manual `systemctl start` after every reboot.
  systemd.services.slurmctld.serviceConfig.Restart = "on-failure";
  systemd.services.slurmctld.serviceConfig.RestartSec = "5s";
  systemd.services.slurmd.serviceConfig.Restart = "on-failure";
  systemd.services.slurmd.serviceConfig.RestartSec = "5s";

  # munge authenticates slurmctld <-> slurmd; the key must exist before munged starts
  # and must not be in the Nix store (that is world-readable). Generate it once, 0400,
  # owned by munge; a rebuild leaves an existing key alone.
  systemd.services.munge-keygen = {
    description = "generate /etc/munge/munge.key if it does not exist";
    wantedBy = [ "munged.service" ];
    before = [ "munged.service" ];
    after = [ "systemd-sysusers.service" ];
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
    script = ''
      set -eu
      if [ ! -s /etc/munge/munge.key ]; then
        install -d -m 0755 -o munge -g munge /etc/munge
        dd if=/dev/urandom of=/etc/munge/munge.key bs=1024 count=1 status=none
        chown munge:munge /etc/munge/munge.key
        chmod 0400 /etc/munge/munge.key
      fi
    '';
  };

  # The NixOS module already puts its `wrappedSlurm` (which sets SLURM_CONF) on PATH;
  # adding pkgs.slurm here would shadow it and then `sinfo` cannot find the config.
}
