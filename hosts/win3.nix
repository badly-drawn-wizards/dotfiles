# win3: the NixOS-WSL Slurm node on the Windows laptop.
#
# Same shared node configuration as win2 (modules/slurm/node-common.nix). win3
# is in `batch` only, not in a `win3` partition: a laptop that sleeps should not
# be a default target. Its CPUs, memory and tailnet address are in
# modules/slurm/inventory.nix.
{ ... }:
{
  imports = [ ../modules/slurm/node-common.nix ];

  slurmCluster.name = "win3";
}
