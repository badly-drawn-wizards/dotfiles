# wsl-flake: the on-node entry point

This directory is **only a thin wrapper**. The node configuration lives in the
main tree:

```
../hosts/win2.nix, ../hosts/win3.nix   which node is being built
../modules/slurm/node-common.nix       WSL, nix, users, ssh/tailnet, toolchain
../modules/slurm.nix                   the Slurm service both roles import
../modules/slurm/inventory.nix         CPUs/memory/features/addresses
```

`flake.nix` imports those files with just nixpkgs + nixos-wsl, so a node can
fetch `...?dir=wsl-flake#win2` and does not evaluate the controller's desktop
inputs (nixvim, wiz, home-manager) or the `path:/workspace/dotfiles-private`
input. The main flake exposes the same systems as
`nixosConfigurations.win2`/`win3`; `nix flake check` there is the CI path.

See **../hosts/README.md** for:

- the hierarchy and what moved out of the old `common.nix`;
- the apply command and per-node prerequisites
  (`/etc/slurm-bootstrap/workspace_ed25519`, tailnet membership);
- why `deck` is an inert `inertConfigurations` output (SteamOS, not NixOS);
- the public/secret classification (addresses and public keys are public;
  the bootstrap key, munge key and tailscale state are out-of-band);
- the no-switch verification commands.

`../hosts/deck.nix` is never a `nixosConfigurations` output, so
`nixos-rebuild switch --flake …#deck` cannot replace SteamOS by accident.
