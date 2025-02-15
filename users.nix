{ config, lib, pkgs, ... }:

{
  security = {
    sudo = {
      enable = true;
      # TODO This doesn't take precedence, I need to investigate.
      extraRules = [
        {
          groups = [ "wheel" ];
          commands = [{ command = "ALL"; options = [ "SETENV" "NOPASSWD" ]; }];
        }
      ];
    };

    wrappers = lib.optionalAttrs config.programs.systemtap.enable {
      stap = {
        source = "${config.boot.kernelPackages.systemtap}/bin/stap";
        owner = "root";
        group = "stapusr";
        setuid = true;
      };
    };
  };

  users = {
    groups = {
      lpadmin = { };
      stapusr = { };
      netdev = { };
      plugdev = { };
      podman = { };
      docker = { };
    };
    users.reuben = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "networkmanager"
        "audio"
        "video"
        "systemd-journal"
        "docker"
        "podman"
        "lpadmin"
        "lp"
        "dialout"
        "wireshark"
        "stapusr"
        "ccache"
        "kvm"
        "libvirtd"
        "netdev"
        "plugdev"
      ];
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  # services.udev.extraRules = ''
  #   KERNEL=="tun", GROUP="netdev", MODE="0777", OPTIONS+="static_node=net/tun"
  # '';

}
