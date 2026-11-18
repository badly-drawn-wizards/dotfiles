# Slurm nodes: one module, one flake

This directory is the node half of the cluster refactor. The controller's half
is `../modules/slurm/controller.nix`, wired from `../slurm.nix`.

```
modules/slurm/constants.nix    address, ports, uids/gids, keys   (one table)
modules/slurm/inventory.nix    every host's CPUs/mem/features…   (one list)
modules/slurm/render.nix       NodeName=/PartitionName= renderer  (one function)
modules/slurm/controller.nix   the controller fragment            (imports the above)
modules/slurm.nix              the module both roles import
wsl-flake/                     this file, common.nix and the node flake
```

`win2` and `win3` are now `nixosConfigurations` in `flake.nix`; there is no
`/etc/nixos-wsl/configuration.nix` to edit on either machine any more.

## Applying a node

The flake is a subdirectory of the `/etc/nixos` git repo, so the node fetches
the controller's commit directly:

```sh
# on win2 (and `#win3` on win3) — NOT on noobnoob
sudo nixos-rebuild switch --flake \
  'git+ssh://reuben@100.76.41.56/etc/nixos?ref=slurm-parity&dir=wsl-flake#win2'
```

Prerequisites, per node, once:

- `reuben` exists with uid 1000 / gid 100 (the NixOS-WSL first-boot user);
- `/etc/slurm-bootstrap/workspace_ed25519` is present, mode 0600 root — the key
  the fetcher and the `/workspace` sshfs use (provisioned by hand; it is
  authorized as `reuben` on the controller);
- the root account can read that key over ssh (put it in `/root/.ssh/config`, or
  set `GIT_SSH_COMMAND` for the one `nixos-rebuild` command);
- `services.tailscale` is enabled (the module does this) and the node is on the
  tailnet, so the controller's `NodeAddr` for it is its own `100.x`.

If fetching over git+ssh is inconvenient, copy this directory and `../modules`
to the node and use `--flake /etc/nixos-wsl#win2`; it must keep the repository
layout because the flake reads `../modules` and `../flake.lock`.

Use `--flake …#win2 --target-host` from the controller only after the node's
`reuben` accepts the controller's key; `nixos-rebuild` over ssh as root is not
configured.

> First switch on a node: the module's supervised `slurm-workspace.service`
> replaces the sshfs mount that was started by hand (its `ExecStartPre` unmounts
> whatever is at `/workspace`). Do the first switch while the node has no
> running jobs.

## What changed from the hand-maintained configs

- **One module**: `../modules/slurm.nix`. The old `configuration.nix` on win2 and
  win3 were copies that drifted (three different `slurm.conf` hashes on
  2026-11-18). A node's CPUs, memory, features, partitions, address and state
  now come from `../modules/slurm/inventory.nix`; the controller's
  `NodeName=`/`PartitionName=` lines are rendered from that same file, so the two
  sides cannot disagree.
- **The refresh is part of the lifecycle, not a manual restart.** The fetcher
  (`slurm-fetch`) resolves the controller's *current* `SLURM_CONF` path with
  `scontrol show config` each time, compares that path and the file's bytes with
  the cached copy, and only then rewrites `/etc/slurm/slurm.conf`. It runs
  before `munged` and `slurmd` at every boot, and it is a store path in the
  node's closure — a node rebuild changes its unit and systemd re-runs it. A
  *controller-only* rebuild does not change the node's closure, so
  `slurm-config-sync.timer` repeats the check every 10 minutes and reloads a
  running slurmd with SIGHUP (no job killed). A munge-key change is the one case
  that restarts `munged` and then `slurmd`, because the key is read at start.
  `slurmCluster.syncInterval = null` disables the timer.
- **A node never generates a munge key.** The controller's keygen creates it
  0400 munge:munge; a node's `munge-keygen` only verifies the fetched key and
  fails loudly rather than authenticating with a key of its own.
- **Slurm ports are opened on `tailscale0` only** (6817/6818), in the module;
  `configuration.nix` no longer lists them.

## The inert `deck` configuration

`flake.nix` also builds an **inert** `deck` system, exposed as
`inertConfigurations.deck` rather than `nixosConfigurations.deck`. The Deck runs
SteamOS: it has no NixOS, no root filesystem to activate and no host-mounted
`/workspace` (its jobs mount it per job via `wiz-tools/deck/deck-ns-exec`). The
entry exists so a future NixOS install has a description to work from; it is
not in use today, and `nixos-rebuild --flake …#deck` cannot replace SteamOS by
accident because it is not a `nixosConfigurations` output. `nix flake check`
only evaluates its Slurm options.

## Verifying without switching

```sh
# on noobnoob: the live host still evaluates
nix eval /etc/nixos#nixosConfigurations.noobnoob.config.system.build.toplevel.drvPath

# the generated controller config is the live one (same store path, same sha256)
nix build --no-link --print-out-paths \
  /etc/nixos#nixosConfigurations.noobnoob.config.services.slurm.etcSlurm
sha256sum /nix/store/9d678ar6…-etc-slurm/slurm.conf   # 39aa62b4…

# on noobnoob: the node flake evaluates and its checks pass
nix flake check --no-build /etc/nixos/wsl-flake
nix eval --raw /etc/nixos/wsl-flake#nixosConfigurations.win2.config.slurmCluster.nodeEntry
```

A full `nix build .#nixosConfigurations.win2.config.system.build.toplevel` is
CI's job or a queued `heavy` job; it is not needed to prove the evaluation.
