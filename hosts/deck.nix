# deck: the Steam Deck. INERT: this is not a NixOS machine.
#
# The Deck runs SteamOS. Its root filesystem is a read-only image, `pacman`
# changes vanish on an OS update, and `deck` cannot create a host /workspace.
# The working Deck node therefore does NOT run a NixOS slurmd: each job enters
# a private user+mount namespace with a per-job sshfs mount of
# noobnoob:/workspace, driven by /workspace/wiz-tools/deck/deck-ns-exec. The
# controller still has a `deck` NodeName= entry (modules/slurm/inventory.nix)
# so `--partition=deck` jobs have somewhere to go, but it is registered DOWN
# until that external slurmd answers.
#
# This file exists only so a future *NixOS* install on the Deck has a
# description to build from. It is exposed as `inertConfigurations.deck`, never
# as `nixosConfigurations.deck`, precisely so `nixos-rebuild switch --flake
# .#deck` cannot replace SteamOS by accident. `nix flake check` evaluates its
# Slurm options; nothing here is switched today.
#
# SteamOS's `hostname -s` is `steamdeck`; inventory.nodeHostname records that so
# the controller can reach an external slurmd. The NixOS name stays `deck`.
{ ... }:
{
  imports = [ ../modules/slurm/node-common.nix ];

  slurmCluster = {
    name = "deck";
    # SteamOS's / is read-only. The Deck mounts /workspace per job inside a
    # user namespace; there is no host mount for NixOS to make.
    workspaceMount.enable = false;
  };

  # node-common defaults this on for the Windows nodes. A NixOS Deck would not
  # be WSL, so turn the NixOS-WSL integration off rather than pretending.
  wsl.enable = false;

  networking.hostName = "deck";
  system.stateVersion = "26.11";
}
