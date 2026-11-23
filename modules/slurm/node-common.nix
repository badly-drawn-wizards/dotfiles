# Common NixOS configuration for a wiz Slurm compute node.
#
# This is the host half of a node: NixOS-WSL, nix, the `reuben` account, sshd
# and the tailnet, the job toolchain, the clock and the memory policy. The
# Slurm half — munge, slurmd, the config/munge-key fetcher, the /workspace
# sshfs service and the tailnet Slurm ports — is ../slurm.nix, which this file
# imports.
#
# A host supplies its Slurm name (and nothing else unless it differs):
#
#   imports = [ ../modules/slurm/node-common.nix ];
#   slurmCluster.name = "win2";
#
# win2 and win3 use this file directly. hosts/deck.nix also imports it for the
# inert future-NixOS description, but turns `wsl.enable` off: the Deck runs
# SteamOS and is not switched by NixOS.
{ pkgs, lib, ... }:
let
  constants = import ./constants.nix;
in
{
  imports = [
    # The Cachix cache name/public key the controller uses. It is a public
    # signing key, not a credential.
    ../../cachix/badlydrawnwizards.nix
    ../slurm.nix
  ];

  # The flake imports nixos-wsl. A host may override the role, but a file that
  # imports node-common is a node unless it says otherwise.
  slurmCluster.role = lib.mkDefault "node";

  # ---- NixOS-WSL ---------------------------------------------------------
  # `mkDefault` so hosts/deck.nix can describe a future NixOS Deck without
  # pretending the current SteamOS install is NixOS.
  wsl.enable = lib.mkDefault true;
  wsl.defaultUser = lib.mkDefault "reuben";
  system.stateVersion = lib.mkDefault "26.11";

  # ---- Clock -------------------------------------------------------------
  # UTC on every node, so a job's timestamps and `date` agree across the
  # cluster regardless of the Windows host's local time zone.
  time.timeZone = lib.mkDefault "UTC";

  # ---- Nix ---------------------------------------------------------------
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

  # ---- Users -------------------------------------------------------------
  # The ids are constants because a file written by a job into /workspace must
  # come back owned by the same numeric user on the controller.
  users.groups.users.gid = constants.gids.users;
  users.users.reuben = {
    isNormalUser = true;
    uid = constants.uids.reuben;
    extraGroups = [
      "wheel"
      "slurm"
    ];
    # The automation key and the human's key(s), from the single table in
    # constants.nix. These are public keys.
    openssh.authorizedKeys.keys = constants.authorizedKeys;
  };

  # ---- SSH and tailnet ---------------------------------------------------
  services.openssh = {
    enable = true;
    # `openFirewall` would open 22 on every interface; the tailscale0 rule
    # below is the only opening (the same policy as the controller).
    openFirewall = false;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ 22 ];

  services.tailscale = {
    enable = true;
  };

  # ---- Job toolchain -----------------------------------------------------
  # Match the controller's build environment (flake.nix devShell + the
  # compiler the host uses): without gcc/binutils a `lake build` on a node
  # compiles Lean but cannot link. `nix build` needs the caches above.
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

  # ---- Memory policy -----------------------------------------------------
  # Cgroups, not overcommit, are the enforcement point: ../slurm.nix runs
  # slurmd with Delegate=Yes and the controller's cgroup.conf sets
  # ConstrainRAMSpace=yes, so a step's memory.max is what bounds it. Overcommit
  # stays heuristic (0), as on the controller: mode 2 would reject the large
  # address-space reservations Lean/Node/Nix make before the cgroup is reached.
  # Stated explicitly so a node does not silently inherit a different mode.
  boot.kernel.sysctl."vm.overcommit_memory" = 0;

  # ---- /workspace --------------------------------------------------------
  # The mount is the supervised sshfs service in ../slurm.nix. Its mount point
  # (/workspace), uid (constants.uids.reuben) and gid (constants.gids.users)
  # are module defaults; `workspaceMount.enable` comes from the inventory entry
  # for this host, so nothing is duplicated here.
}
