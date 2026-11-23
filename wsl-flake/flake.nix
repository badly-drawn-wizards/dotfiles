# Thin on-node entry point for the NixOS-WSL Slurm nodes (win2, win3).
#
# The configuration is NOT here. The host files and the shared modules live in
# the main configuration:
#
#   hosts/win2.nix, hosts/win3.nix      which host is being built
#   modules/slurm/node-common.nix       WSL, nix, users, ssh/tailnet, toolchain
#   modules/slurm.nix                   the Slurm service (munge, slurmd, fetcher,
#                                       /workspace) both roles import
#   modules/slurm/inventory.nix         CPUs/memory/features/addresses, also read
#                                       by the controller's slurm.conf renderer
#
# The main flake exposes the same nodes as `nixosConfigurations.win2`/`win3`
# from those same files, so the controller and CI can build them. This wrapper
# exists so a node can fetch just `...?dir=wsl-flake#win2` and does not have to
# evaluate the controller's full input set: nixvim, wiz, home-manager and the
# `path:/workspace/dotfiles-private` input need not exist on a node.
#
# Apply on a node (do NOT run this on noobnoob):
#
#   sudo nixos-rebuild switch --flake \
#     'git+ssh://reuben@100.76.41.56/etc/nixos?ref=agent/slurm-nodes&dir=wsl-flake#win2'
#
# and `#win3` for the laptop. The git+ssh form means the node fetches the same
# commit the controller's repo has; there is no copied /etc/nixos-wsl to drift.
# The node's root account must be able to read the controller as `reuben` (the
# bootstrap key at /etc/slurm-bootstrap/workspace_ed25519 is one way; put it in
# /root/.ssh/config or pass GIT_SSH_COMMAND). Alternatively copy this directory
# once and use `--flake /etc/nixos-wsl#win2`, keeping the repo layout: ../hosts,
# ../modules and ../flake.lock are read.
#
# KNOWN LIMITATION, on purpose: this flake reads `../flake.lock` for the
# controller's nixpkgs revision. It is not a standalone flake; it is a
# subdirectory of /etc/nixos. That is what makes the protocol versions match
# without a second pin (see the `nixpkgs-matches-controller` check below).
{
  description = "On-node entry point for the NixOS-WSL Slurm nodes (win2, win3)";

  inputs = {
    # nixos-unstable, pinned by flake.lock to the controller's revision. The
    # slurm/munge versions must match slurmctld's (26.05.x), and jobs name store
    # paths, so the two stores should be the same nixpkgs.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # `wsl.enable` and the distro integration. follows keeps one nixpkgs.
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/05721a428830972f025a8575b1a19c418d422d1b";
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

      # The controller's own nixpkgs pin. Used only by the drift check; the
      # node's inputs are pinned by this flake's lock.
      controllerLock = builtins.fromJSON (builtins.readFile ../flake.lock);
      controllerNixpkgsRev = controllerLock.nodes.${controllerLock.nodes.root.inputs.nixpkgs}.locked.rev;

      # The host file, node-common and the Slurm module are the main config's
      # files; this flake only chooses the input set they are evaluated with.
      mkNode =
        name:
        lib.nixosSystem {
          inherit system;
          modules = [
            nixos-wsl.nixosModules.default
            (../hosts + "/${name}.nix")
          ];
        };

      # INERT. See ../hosts/deck.nix: SteamOS is not NixOS and is not switched.
      # This is deliberately not a `nixosConfigurations` output.
      deck = mkNode "deck";
    in
    {
      nixosConfigurations = {
        win2 = mkNode "win2";
        win3 = mkNode "win3";
      };

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
