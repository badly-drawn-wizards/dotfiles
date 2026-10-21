{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./boot.nix
      ./filesystems.nix
      ./printing.nix
      ./bluetooth.nix
      ./joycon.nix
      ./logitune.nix
    ];

  # ~15G usable RAM vs 32 threads: the old max-jobs 8 x cores 0 (= all 32)
  # allowed 256 compiler processes and thrashed swap until the box froze.
  nix.settings = {
    max-jobs = 4; # parallel derivations
    cores = 8; # threads per derivation's make/ninja
  };

  # Cap the daemon's cgroup (all builds run under it): past MemoryHigh the
  # kernel reclaims from builds first; past MemoryMax the build is OOM-killed
  # and fails, instead of the whole desktop swapping to a standstill.
  systemd.services.nix-daemon.serviceConfig = {
    MemoryHigh = "9G";
    MemoryMax = "11G";
    MemorySwapMax = "2G";
  };
  # Builds yield CPU and disk to the desktop.
  nix.daemonCPUSchedPolicy = "batch";
  nix.daemonIOSchedClass = "idle";

  # Compressed swap in RAM, used before the encrypted disk swap.
  zramSwap = {
    enable = true;
    memoryPercent = 50;
  };
  #powerManagement.cpuFreqGovernor = "powersave";

  hardware = {
    enableAllFirmware = true;
    enableRedistributableFirmware = true;
    cpu.amd = {
      updateMicrocode = true;
    };
  };


  services.udev = {
    extraRules = ''
      KERNEL=="ttyUSB*", MODE="0777"
    '';
  };

}
