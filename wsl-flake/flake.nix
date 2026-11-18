# The nodes of the wiz Slurm cluster, as NixOS-WSL systems.
#
# There is ONE Slurm module: ../modules/slurm.nix. This flake only says which
# host is being built; CPUs, memory, features, partitions and the tailnet
# address come from ../modules/slurm/inventory.nix, the same file the controller
# renders its slurm.conf from.
#
# Apply on a node (do NOT run this on noobnoob):
#
#   sudo nixos-rebuild switch --flake \
#     'git+ssh://reuben@100.76.41.56/etc/nixos?ref=slurm-parity&dir=wsl-flake#win2'
#
# and `#win3` for the laptop. The git+ssh form means the node fetches the same
# commit the controller's repo has; there is no copied /etc/nixos-wsl to drift.
# The node's root account must be able to read the controller as `reuben` (the
# bootstrap key already installed at /etc/slurm-bootstrap/workspace_ed25519 is
# one way; put it in /root/.ssh/config or pass GIT_SSH_COMMAND). Alternatively
# copy this directory once and use `--flake /etc/nixos-wsl#win2`, keeping the
# repo layout: ../modules and ../flake.lock are read.
#
# KNOWN LIMITATION, on purpose: this flake reads `../flake.lock` for the
# controller's nixpkgs revision. It is not a standalone flake; it is a
# subdirectory of /etc/nixos. That is what makes the protocol versions match
# without a second pin (see the `nixpkgs-matches-controller` check below).
{
  description = "NixOS-WSL nodes (win2, win3) of the wiz Slurm cluster";

  inputs = {
    # nixos-unstable, pinned by flake.lock to the controller's revision. The
    # slurm/munge versions must match slurmctld's (26.05.x), and jobs name store
    # paths, so the two stores should be the same nixpkgs.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # `wsl.enable` and the distro integration. follows keeps one nixpkgs.
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixos-wsl,
    }:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
      pkgs = nixpkgs.legacyPackages.${system};

      inventory = import ../modules/slurm/inventory.nix;

      # The controller's own nixpkgs pin. Used only by the drift check; the
      # node's inputs are pinned by this flake's lock.
      controllerLock = builtins.fromJSON (builtins.readFile ../flake.lock);
      controllerNixpkgsRev = controllerLock.nodes.${controllerLock.nodes.root.inputs.nixpkgs}.locked.rev;

      nixos = modules: lib.nixosSystem { inherit system modules; };

      mkNode =
        name:
        nixos [
          nixos-wsl.nixosModules.default
          ./common.nix
          ../modules/slurm.nix
          {
            slurmCluster = {
              role = "node";
              inherit name;
              # Everything else (cpus, realMemoryMb, features, partition, ...)
              # defaults from inventory.${name} inside the module.
            };
          }
        ];

      # INERT. The Steam Deck runs SteamOS, not NixOS, and is NOT being
      # switched: its jobs come from wiz-tools/deck/deck-ns-exec, not from a
      # NixOS slurmd. This entry exists only so a future NixOS install has a
      # description to build from. It is deliberately not a
      # `nixosConfigurations` output, so `nixos-rebuild switch --flake ...#deck`
      # cannot replace SteamOS by accident; the only thing CI does with it is
      # evaluate its Slurm options (see `checks.deck-is-evaluable`).
      deck = nixos [
        ../modules/slurm.nix
        {
          slurmCluster = {
            role = "node";
            name = "deck";
            # SteamOS's / is read-only, so the Deck mounts /workspace per job
            # inside a user namespace; there is no host mount to make.
            workspaceMount.enable = false;
          };
          system.stateVersion = "26.11";
        }
      ];
    in
    {
      nixosConfigurations = {
        win2 = mkNode "win2";
        win3 = mkNode "win3";
      };

      # Not a nixosConfiguration on purpose: see the comment above.
      inertConfigurations.deck = deck;

      checks.${system} = {
        # `nix flake check` must fail, not warn, if the node's nixpkgs drifts
        # from the controller's: an older/newer slurm refuses the other side
        # with "Protocol version mismatch". Fix with:
        #   nix flake lock --override-input nixpkgs github:NixOS/nixpkgs/<rev>
        nixpkgs-matches-controller =
          if nixpkgs.rev == controllerNixpkgsRev then
            pkgs.runCommand "nixpkgs-matches-controller" { } "touch $out"
          else
            throw ''
              wsl-flake's nixpkgs (${nixpkgs.rev}) is not the controller's
              (${controllerNixpkgsRev}). slurmctld and slurmd would be built from
              different revisions. Re-pin:

                nix flake lock --override-input nixpkgs github:NixOS/nixpkgs/${controllerNixpkgsRev}
            '';

        # Forces the inert deck configuration to evaluate without requiring a
        # fake filesystem/bootloader (which is why it is not a toplevel check).
        deck-is-evaluable = pkgs.writeText "deck-node-entry" deck.config.slurmCluster.nodeEntry;
      };
    };
}
