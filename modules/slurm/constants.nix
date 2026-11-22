# Cluster-wide constants for the wiz Slurm cluster.
#
# This file is the *only* copy of these values. It is a plain attrset (no NixOS
# arguments) because it is imported by three different evaluators:
#
#   * modules/slurm.nix            — the NixOS module (both roles)
#   * modules/slurm/controller.nix — the controller fragment
#   * modules/slurm/inventory.nix  — the host list, which the node flake reads
#
# The node flake reaches it through the inventory, so the address, the ports,
# the ids and the keys cannot drift between the controller and the nodes by a
# copy-paste edit.
let
  # The `wiznode@CAREL-ROG` automation key the Windows nodes already use.
  automationKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILR99yRVeczI2Ow45kyvlDnhdDFckweOMnPEd8mCvwsz wiznode@CAREL-ROG slurm automation";

  # The human's own keys. These are what let the controller `ssh win2` and
  # `nix copy --to ssh-ng://win2` without inventing a third key.
  reubenKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMn/ObOslXcEueiBA86tmMoxuYilN5/ey3A9H7Mn+Ip7 reuben@noobnoob"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC8PRKuzxVZ/Yst4XpfcR5vhsOK4eOI7YI3ykMUqnxObXmE4zHs5n1ec+v6l3dhM6t2lNbAiVZxr7ZKclgQoBHUmw38sYlAs0AYISA/dTZE3h0kaedINBVsjjvYFHWX6NSjdTzm+idga240m9GKWBvNem5eXtZg4AiXg3orRjt7DXFGdPMt0OKKPaAOVYo8b1P4TTqToBVWz3mPFhG7FzrdUlkGEnscxjz0yDrCeoj3yngxOLlypFr9h/hhQhUKfmQDSFbgq4ZZW48DsHHh+xDwnWQiYjQdf4fU6vN6BhGyQ4FaSEiol2drZGvdRGwHLFHyJP8+Bdhn9ZrN8QASKiSS4jR6ki7VoKmTiQHR4Sdlyt+Z7d7phRS004yIVdWM/Mq+7lJVPyyWT/ZCmmCZn/40x1naJymk8YVBvRAhVAHDDDM3Rnsoalgi7JLXwXHOfxCDtUvorPu6mz3cklYjJ+3DTs/ptYG7Nv/fMV48Xr5eGHnlscVHrp+WuItIhGGEN8E= reuben@noobnoob"
  ];
in
{
  # The controller is reached by tailnet address, not MagicDNS: a node that has
  # just booted may not have resolved the tailnet's DNS yet.
  controllerHost = "noobnoob";
  controllerAddr = "100.76.41.56";
  clusterName = "noobnoob";

  ports = {
    slurmctld = 6817;
    slurmd = 6818;
  };

  # The ids are the same on every host so that files a job writes (in
  # /workspace) come back owned by the same numeric user. These are NixOS's
  # static ids; the module asserts that config.ids agrees with them.
  uids = {
    munge = 989;
    slurm = 307;
    reuben = 1000;
  };
  gids = {
    munge = 980;
    slurm = 307;
    users = 100;
  };

  # Where a node keeps the private key it uses to reach the controller as
  # `reuben` (slurm.conf, the munge key, and the /workspace sshfs). The key is
  # provisioned by hand once; only its path is configuration.
  bootstrapUser = "reuben";
  bootstrapKey = "/etc/slurm-bootstrap/workspace_ed25519";

  # The two files the fetcher writes on a node. The controller never has an
  # /etc/slurm: its config is a /nix/store path from services.slurm.
  paths = {
    slurmConf = "/etc/slurm/slurm.conf";
    # The controller's Slurm module renders cgroup.conf beside slurm.conf; a node
    # must fetch it too, or slurmd starts with Slurm's defaults (ConstrainCores=no,
    # ConstrainRAMSpace=no) and `--mem`/`-c` are advisory on that node.
    cgroupConf = "/etc/slurm/cgroup.conf";
    mungeKey = "/etc/munge/munge.key";
  };

  keys = {
    automation = automationKey;
    reuben = reubenKeys;
    # The Deck's key. It is authorized on the controller (not on the nodes) and
    # is recorded here so the cluster's keys stay in one table.
    deck = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcpgdwnkrV5JAZcbNcLtI85/wsm7QbrxKmnR88zXtKm deck-to-noobnoob-worktrees";
  };

  # What a node's `reuben` account accepts. Derived, never repeated.
  authorizedKeys = [ automationKey ] ++ reubenKeys;
}
