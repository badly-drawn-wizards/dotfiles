{ config, lib, pkgs, ... }:

{
  security = {
    sudo = {
      enable = true;
      # TODO This doesn't take precedence, I need to investigate.
      extraRules = [
        {
          groups = [ "wheel" ];
          commands = [{ command = "ALL"; options = ["SETENV" "NOPASSWD"]; }];
        }
      ];
    };

    wrappers.stap = {
      source = "${config.boot.kernelPackages.systemtap}/bin/stap";
      owner = "root";
      group = "stapusr";
      setuid = true;
    };
  };

  users = {
    groups = {
      lpadmin = {};
      stapusr = {};
    };
    users.reuben = {
      isNormalUser = true;
      extraGroups = [
        "wheel" "networkmanager" "audio" "video"
        "systemd-journal" "docker" "lpadmin" "dialout"
        "wireshark" "stapusr" "ccache"];
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

}
