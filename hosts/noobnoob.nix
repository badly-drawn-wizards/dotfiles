# noobnoob: the controller (and a `batch` node) of the wiz Slurm cluster.
#
# The desktop configuration is still ../configuration.nix, which imports
# ../slurm.nix and therefore modules/slurm/controller.nix. This file is only the
# host entry point so every host in the cluster has one place in hosts/.
{ ... }:
{
  imports = [ ../configuration.nix ];
}
