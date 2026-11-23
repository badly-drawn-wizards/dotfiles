# Slurm hosts: one hierarchy

```
hosts/noobnoob.nix              controller (imports ../configuration.nix)
hosts/win2.nix                  NixOS-WSL node: Slurm name only
hosts/win3.nix                  NixOS-WSL node: Slurm name only
hosts/deck.nix                  INERT future-NixOS description (SteamOS today)

modules/slurm/node-common.nix   node host: WSL, nix, users, ssh/tailnet,
                                toolchain, overcommit, /workspace notes
modules/slurm.nix               Slurm service for both roles: options, munge,
                                slurm ids, slurmd, the config/munge-key fetcher,
                                the sync timer, the /workspace sshfs service,
                                the tailnet Slurm ports
modules/slurm/controller.nix    controller fragment; imports ../slurm.nix
modules/slurm/inventory.nix     every host's CPUs/memory/features/address
modules/slurm/constants.nix     address, ports, uids/gids, public keys
modules/slurm/render.nix        NodeName=/PartitionName= renderer
```

The controller's `NodeName=`/`PartitionName=` lines are rendered from
`inventory.nix`; a node host reads its own entry from the same file. A host's
Slurm name is therefore the only thing in `hosts/win2.nix` and
`hosts/win3.nix`, and the two sides cannot disagree about CPUs, memory or
features.

## Where the node systems are defined

Both places use the same files; neither has a second copy of the config:

- the main flake exposes `nixosConfigurations.win2` and `win3` (for the
  controller, CI and `nix build`), built with the plain `nixpkgs` input;
- `wsl-flake/` is a **thin on-node entry point**. A node switches with
  `...?dir=wsl-flake#win2`, so it does not need to evaluate the controller's
  desktop inputs (nixvim, wiz, home-manager) or the
  `path:/workspace/dotfiles-private` input.

The main flake's `nix flake check` reaches the same node systems; it stops
later, on the pre-existing `packages` output (see **Verifying** below). The
wrapper's `nixpkgs-matches-controller` check fails if its nixpkgs pin drifts
from the controller's; both locks currently resolve to the same revision.

## Applying a node

On win2 (and `#win3` on win3), NOT on noobnoob:

```sh
sudo nixos-rebuild switch --flake \
  'git+ssh://reuben@100.76.41.56/etc/nixos?ref=agent/slurm-nodes&dir=wsl-flake#win2'
```

Prerequisites, per node, once:

- `reuben` exists with uid 1000 / gid 100 (the NixOS-WSL first-boot user;
  `node-common.nix` declares the ids so they do not drift);
- `/etc/slurm-bootstrap/workspace_ed25519` is present, mode 0600 root — the
  private key the fetcher and the `/workspace` sshfs use. It is provisioned by
  hand and authorized as `reuben` on the controller; only its path is in the
  public repo (see **Secrets** below);
- the root account can read that key over ssh (put it in `/root/.ssh/config`, or
  set `GIT_SSH_COMMAND` for the one `nixos-rebuild` command);
- `services.tailscale` is enabled (the node profile does this) and the node is
  on the tailnet, so the controller's `NodeAddr` for it is its own `100.x`.

If fetching over git+ssh is inconvenient, copy `wsl-flake/`, `hosts/`,
`modules/` and the root `flake.lock` to the node in the same layout and use
`--flake /etc/nixos-wsl#win2`.

Use `--flake …#win2 --target-host` from the controller only after the node's
`reuben` accepts the controller's key; `nixos-rebuild` over ssh as root is not
configured.

> First switch on a node: the module's supervised `slurm-workspace.service`
> replaces the sshfs mount that was started by hand (its `ExecStartPre` unmounts
> whatever is at `/workspace`). Do the first switch while the node has no
> running jobs.

## What moved out of the old node configs

- **One module hierarchy.** `hosts/win2.nix` and `hosts/win3.nix` set only
  `slurmCluster.name`; `modules/slurm/node-common.nix` owns the host
  configuration that used to be duplicated in `wsl-flake/common.nix` and in
  each node's `/etc/nixos-wsl/configuration.nix`: `wsl.enable`, the nix settings
  and trusted users, the `reuben` account and authorized public keys, sshd
  (key-only, tailnet only), tailscale, the job toolchain, UTC and the
  overcommit/cgroup policy.
- **Slurm stays in `modules/slurm.nix`**: munge, the slurm ids, the slurmd unit
  and its cgroup delegation, the `slurm-bootstrap` fetcher, the
  `slurm-config-sync` timer, the `/workspace` sshfs service and the
  tailscale0-only Slurm ports. The node-only block now keeps only what slurmd
  itself needs (the slurm system user, the binaries, `SLURM_CONF`, the units).
- **The controller is unchanged** except for its entry point moving to
  `hosts/noobnoob.nix`, which imports the existing `configuration.nix`. Its
  generated `slurm.conf` is byte-identical to the pre-refactor one.

## The inert `deck` configuration

`hosts/deck.nix` is **not** a switchable NixOS host. The Deck runs SteamOS: a
read-only image root, no NixOS to activate, and no host `/workspace` (each job
mounts it inside a user+mount namespace via `wiz-tools/deck/deck-ns-exec`). The
working Deck node is that external mechanism, not this file.

The description is exposed as `inertConfigurations.deck`, never
`nixosConfigurations.deck`, so `nixos-rebuild switch --flake .#deck` cannot
replace SteamOS by accident. `nix flake check` only evaluates its Slurm
options. It imports `node-common.nix` with `wsl.enable = false` and
`slurmCluster.workspaceMount.enable = false`, so a future NixOS install has a
starting point rather than a claim that the current Deck is NixOS.

## Secrets

Public, and deliberately kept in the public repo (it is a public repository):

| data | location | why it is public |
|---|---|---|
| Tailnet addresses (`100.76.41.56`, `100.93.9.54`, `100.91.219.16`, `100.106.233.3`) | `modules/slurm/constants.nix`, `modules/slurm/inventory.nix` | Tailscale addresses are not credentials; reachability is enforced by the tailnet's ACLs and device authorization. They are needed to render `slurm.conf` and to document the topology. |
| SSH **public** keys (automation, `reuben`, deck) | `modules/slurm/constants.nix` | Public keys are meant to be published; they are the `authorized_keys` entries. |
| Cachix cache name and public signing key | `cachix/badlydrawnwizards.nix` | A binary-cache verification key is public by construction; it cannot sign anything. |

Not in either repo, by design:

| secret | where it lives | why not here |
|---|---|---|
| Bootstrap SSH private key `/etc/slurm-bootstrap/workspace_ed25519` | provisioned by hand on each node, root 0600 | Only its path is configuration. Putting the key in sops would require distributing the sops age private key to every node, which is a larger trust change than the one-time hand provisioning. |
| Munge key `/etc/munge/munge.key` | generated on the controller, fetched by nodes over ssh before `munged` starts | It authenticates the whole cluster; the module deliberately never generates a node-local key. |
| Tailscale node state / any auth key | `/var/lib/tailscale/tailscaled.state`, node-local | `tailscaled` joins once and persists; no auth token is declared. |
| Sops age private key | `/var/lib/sops-nix/key.txt` on noobnoob (root 0600), backed up in KeePassXC | The private repo's `modules/sops.nix` uses it for the controller's VPN secrets. |

No node configuration in this change needs a sops secret. If a future node
credential (for example a tailnet auth key, or a declarative bootstrap key) is
wanted, add it to `dotfiles-private/secrets/` with the existing `.sops.yaml`
age recipient and a **dedicated** private module; do not import the whole
`dotfiles-private.nixosModules.default` on a node, because it also carries the
controller's OpenVPN client profiles and expects the controller's sops key.

## Verifying without switching

```sh
# the node entry point: this is the check that covers the node systems
nix flake check ./wsl-flake

# evaluation of the node systems in the main flake
nix eval --raw .#nixosConfigurations.win2.config.slurmCluster.nodeEntry
nix eval --raw .#nixosConfigurations.win3.config.slurmCluster.nodeEntry
nix eval --raw .#inertConfigurations.deck.config.slurmCluster.nodeEntry

# build a node's system (no switch)
nixos-rebuild build --flake .#win2
nixos-rebuild build --flake .#win3
# or: nix build .#nixosConfigurations.win2.config.system.build.toplevel

# the controller's generated config, compared with the live one
nix build --no-link --print-out-paths \
  .#nixosConfigurations.noobnoob.config.services.slurm.etcSlurm
sha256sum /nix/store/…-etc-slurm/slurm.conf
```

A bare `nix flake check` at the repository root also walks the existing
`packages.x86_64-linux` output (the whole nixpkgs attrset) and stops at
`packages.x86_64-linux.name`; that is a pre-existing desktop-flake issue,
identical at the parent commit, and unrelated to the Slurm hosts. The wrapper
check and the targeted commands above cover the node systems.

The pre-refactor `slurm.conf` used in this change was
`39aa62b4214f29acb7eac94ad8a1bd7058164b037c59ad9984b06d39e7759eaa`; the
inventory drift check in the change report compares it, the node lines and the
partition lines.
