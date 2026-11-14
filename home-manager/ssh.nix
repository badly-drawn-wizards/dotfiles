{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    settings = {
      "ssh.dev.azure.com vs-ssh.visualstudio.com *.visualstudio.com" = {
        identitiesOnly = true;
        identityFile = [
          "~/.ssh/id_rsa"
        ];
      };
      # millionants' initrd, to enter its LUKS passphrase. The initrd only
      # has root, and its key is restricted to the passphrase prompt. The
      # public name works from the LAN too (the router loops it back).
      "millionants-initrd" = {
        hostname = "millionants.badlydrawnwizards.com";
        port = 2222;
        user = "root";
        requestTTY = "yes";
        remoteCommand = "systemd-tty-ask-password-agent";
      };
      # Automation to the aarch64 builder: a passphrase-less key, so background
      # `ssh millionants` / Nix copy jobs never open a KeePassXC prompt.
      "millionants" = {
        hostname = "millionants";
        user = "reuben";
        identitiesOnly = true;
        identityFile = [ "~/.ssh/millionants_auto" ];
      };
      "*" = {
        identitiesOnly = true;
        identityFile = [
          "~/.ssh/id_ed25519"
        ];
      };
    };
  };
}
