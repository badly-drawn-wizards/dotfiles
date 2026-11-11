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

  # Keep parallel compilers within what RAM can hold.
  nix.settings = {
    max-jobs = 4; # parallel derivations
    cores = 8; # threads per derivation's make/ninja
  };

  # Cap the daemon's cgroup (all builds run under it): past MemoryHigh the
  # kernel reclaims from builds first; past MemoryMax the build is OOM-killed
  # rather than the desktop swapping to a halt.
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
      # Lattice iCEstick (FTDI FT2232H): let iceprog open the raw USB device.
      SUBSYSTEM=="usb", ATTR{idVendor}=="0403", ATTR{idProduct}=="6010", MODE="0660", GROUP="plugdev", TAG+="uaccess"
    '';
  };

}
