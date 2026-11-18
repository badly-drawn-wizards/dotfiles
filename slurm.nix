# Slurm, single controller, for the heavy build queue (2026-11-18).
#
# This file is only the controller's *tuning*. The role, the shared ids, the
# munge key lifecycle, the firewall openings and the slurm.conf's
# NodeName=/PartitionName= lines come from modules/slurm/:
#
#   modules/slurm/controller.nix  — renders every node and partition line from
#                                   modules/slurm/inventory.nix (one host list)
#   modules/slurm.nix             — the options, munge, the slurm user, the
#                                   firmware-independent service settings
#   modules/slurm/constants.nix   — address, ports, ids, keys (one table)
#
# The nodes are described in the inventory, not here: win2/win3/deck each read
# their own entry from the same file in wsl-flake/flake.nix, so the controller
# and the node cannot disagree about a node's CPUs, memory or features.
#
# Accounting: Slurm reports MiB. The box has 27 GiB; RealMemory leaves room for
# the OS, the desktop session and nix-daemon (which runs outside any job).
# `heavy`'s own budget is 16 G; the node is sized so a job can be larger than
# that when the queue is quiet (`--mem 20G`), which is the point of having a
# real scheduler.
{ ... }:
{
  imports = [ ./modules/slurm/controller.nix ];

  # The key into modules/slurm/inventory.nix; also this host's Slurm node name.
  slurmCluster.name = "noobnoob";

  services.slurm = {
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
}
