# Host configuration for the NixOS-WSL nodes (win2, win3): everything that is
# the same on both and is not Slurm.
#
# Slurm itself — munge, the slurm user, slurmd, the fetcher, /workspace — is in
# ../modules/slurm.nix. The per-host facts (CPUs, memory, features, partition,
# address) are in ../modules/slurm/inventory.nix, read by the flake.
{ pkgs, ... }:
{
  imports = [
    # One copy of the Cachix cache name/key, shared with the controller.
    ../cachix/badlydrawnwizards.nix
  ];

  wsl.enable = true;
  wsl.defaultUser = "reuben";
  time.timeZone = "UTC";
  # NixOS-WSL asks for a user on first boot; this keeps a rebuilt system from
  # changing it.
  system.stateVersion = "26.11";

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    # A job may run `nix build`/`nix copy` as the submitting user, not only as
    # root.
    trusted-users = [
      "root"
      "reuben"
    ];
  };

  # Match the controller's build environment (flake.nix devShell + the compiler
  # the host uses): without gcc/binutils a `lake build` on this node compiles
  # Lean but cannot link. `nix build` needs the caches above.
  slurmCluster.extraPackages = with pkgs; [
    git
    cacert
    elan
    gcc
    binutils
    gnumake
    pkg-config
    gmp
    boehmgc
    llvm
    python3
    z3
    cadical
    yosys
    nodejs
    coreutils
    gnugrep
    gnused
    findutils
    diffutils
    which
    bashInteractive
  ];
}
