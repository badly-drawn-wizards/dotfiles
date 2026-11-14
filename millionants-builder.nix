{ pkgs, ... }:

{
  # millionants: an aarch64 NixOS host on the tailnet (8 cores, 31 GB). Used as a
  # remote builder for aarch64 derivations (the Wiz toolchain/tests on ARM); it is not
  # a GitHub Actions runner. The daemon runs the ssh as root, hence sshKey below; the
  # host key is in /etc/ssh/ssh_known_hosts via programs.ssh.knownHosts.
  nix.distributedBuilds = true;
  nix.settings.trusted-users = [ "root" "@wheel" ];
  nix.buildMachines = [
    {
      hostName = "millionants";
      sshUser = "reuben";
      sshKey = "/root/.ssh/millionants";
      protocol = "ssh-ng";
      system = "aarch64-linux";
      maxJobs = 8;
      speedFactor = 2;
      supportedFeatures = [ "big-parallel" "kvm" ];
    }
  ];

  programs.ssh.knownHosts.millionants = {
    hostNames = [ "millionants" "millionants.tail8b69b9.ts.net" "100.117.118.54" ];
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF3hIaOf06GrPwREQ0peAf2QcVGrOyESMQ6UE9oZWlJU";
  };
}
