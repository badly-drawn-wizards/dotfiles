# The controller half of the Slurm module.
#
# It turns the single host inventory (modules/slurm/inventory.nix) into the
# `NodeName=` and `PartitionName=` lines of the cluster's slurm.conf. Neither
# line is written in /etc/nixos/slurm.nix any more: a host is described in
# exactly one place, and that place is also what each node host reads for its
# own CPUs, memory and features.
#
# Imported by /etc/nixos/slurm.nix. The host-specific tuning (extraConfig,
# cgroup settings) stays there; this file owns only what is derived from the
# inventory and the shared constants.
{ config, lib, ... }:
let
  cfg = config.slurmCluster;
  constants = import ./constants.nix;
  inventory = import ./inventory.nix;
  render = import ./render.nix { inherit lib inventory; };
in
{
  imports = [ ../slurm.nix ];

  # A node host sets `role = "node"` explicitly, which wins over this.
  slurmCluster.role = lib.mkDefault "controller";

  services.slurm = {
    server.enable = true; # slurmctld
    client.enable = true; # slurmd (the controller is also a node)

    controlMachine = cfg.controllerHost;
    controlAddr = cfg.controllerAddr;
    clusterName = constants.clusterName;

    # Derived from the inventory, in inventory order. `batch` is
    # noobnoob,win2,win3 because those are the nodes whose `partition` list
    # contains "batch"; it is not a second list to maintain.
    nodeName = map render.renderNode inventory.nodes;
    partitionName = map render.renderPartition inventory.partitions;

    # cgroup v2 for tracking and for the memory cap; spank-free.
    procTrackType = "proctrack/cgroup";
  };
}
