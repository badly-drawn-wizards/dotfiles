# win2: the NixOS-WSL Slurm node on the Windows desktop.
#
# Everything except the Slurm name is shared with win3 in
# modules/slurm/node-common.nix; the CPUs, memory, features, partition and
# tailnet address for this host come from modules/slurm/inventory.nix. The
# controller renders its NodeName= line from the same entry, so this file must
# not repeat any of those numbers.
{ ... }:
{
  imports = [ ../modules/slurm/node-common.nix ];

  slurmCluster.name = "win2";
}
