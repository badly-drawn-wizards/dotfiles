# The cluster's hosts, described ONCE.
#
# The controller fragment (modules/slurm/controller.nix) turns this list into the
# `NodeName=`/`PartitionName=` lines of slurm.conf, and each node host
# (hosts/win2.nix, hosts/win3.nix, the inert hosts/deck.nix) reads the same
# entry for the node it is building. There is therefore no second place where
# win2's CPUs, win3's memory or deck's state can be edited: change the entry
# here and the controller's config and the node's module options move together.
#
# Field meanings:
#   addr          tailnet address the controller dials (null: the controller)
#   port          slurmd port; 6818 on the nodes, absent on the controller
#   nodeHostname  what `hostname -s` returns there when it differs from the name
#   topology      Sockets/CoresPerSocket/ThreadsPerCore, if advertised
#   weight        Slurm placement weight (noobnoob is preferred)
#   state         the boot-time state the controller records
#   partition     partitions the node belongs to (its own plus a shared pool)
#   workspace     whether the module mounts the controller's /workspace via sshfs
#   comment       why the numbers are what they are; kept next to the numbers
let
  constants = import ./constants.nix;

  nodes = [
    {
      name = "noobnoob";
      addr = null;
      port = null;
      nodeHostname = null;
      # CPUs= is what Slurm may allocate: 0-23 are for jobs, 24-31 are reserved
      # for the desktop's perf window, so advertise 24 rather than `nproc` (32).
      cpus = 24;
      topology = null;
      # 27 GiB total; 24 GiB is the largest a job may reserve, so ~3 GiB stays
      # with the OS, the desktop session and nix-daemon.
      realMemoryMb = 24000;
      features = [ ];
      weight = 100;
      state = "UNKNOWN";
      partition = [ "batch" ];
      workspace = false;
      comment = "the controller and the preferred batch host";
    }
    {
      name = "deck";
      addr = "100.106.233.3";
      port = null;
      # SteamOS's `hostname -s` is `steamdeck`; slurmd there is given -N deck, and
      # this is how the controller finds it.
      nodeHostname = "steamdeck";
      cpus = 8;
      topology = null;
      realMemoryMb = 14000;
      features = [ "steamdeck" ];
      weight = null;
      # Registered DOWN so the partition exists and a job can be aimed at it,
      # but nothing is scheduled until its slurmd answers. `scontrol update
      # NodeName=deck State=RESUME` after that.
      state = "DOWN";
      partition = [ "deck" ];
      # SteamOS has a read-only /, so the deck mounts /workspace per job inside
      # a user namespace (wiz-tools/deck/deck-ns-exec), not as a host mount.
      workspace = false;
      comment = "the Steam Deck; SteamOS, not NixOS";
    }
    {
      name = "win2";
      addr = "100.93.9.54";
      port = constants.ports.slurmd;
      nodeHostname = "win2";
      # 20 of the host's 24 vCPUs (Windows keeps the rest).
      cpus = 20;
      topology = {
        sockets = 1;
        coresPerSocket = 10;
        threadsPerCore = 2;
      };
      # Measured against the WSL2 VM, not the 32 GB host.
      realMemoryMb = 15800;
      features = [ "wsl2" ];
      weight = null;
      state = "UNKNOWN";
      partition = [ "batch" "win2" ];
      workspace = true;
      comment = "NixOS-WSL on the desktop";
    }
    {
      name = "win3";
      addr = "100.91.219.16";
      port = constants.ports.slurmd;
      nodeHostname = "win3";
      cpus = 20;
      topology = {
        sockets = 1;
        coresPerSocket = 10;
        threadsPerCore = 2;
      };
      # 22000 MiB of a 24 GiB cap: the cap is a ceiling, not a reservation, so
      # leave Windows headroom.
      realMemoryMb = 22000;
      features = [ "wsl2" ];
      weight = null;
      state = "UNKNOWN";
      # No `win3` partition: a laptop that sleeps should not be a default target.
      partition = [ "batch" ];
      workspace = true;
      comment = "NixOS-WSL on the laptop";
    }
  ];
in
{
  inherit nodes;

  # Lookup for the node hosts and for the module's defaults.
  byName = builtins.listToAttrs (map (n: { name = n.name; value = n; }) nodes);

  # Partition parameters. Membership is derived from each node's `partition`
  # list, in `nodes` order, so `batch` is `noobnoob,win2,win3` without a second
  # place to list them.
  partitions = [
    {
      name = "batch";
      default = true;
      maxTime = "INFINITE";
      state = "UP";
    }
    {
      name = "deck";
      default = false;
      maxTime = "04:00:00";
      state = "UP";
    }
    {
      name = "win2";
      default = false;
      maxTime = "04:00:00";
      state = "UP";
    }
  ];
}
