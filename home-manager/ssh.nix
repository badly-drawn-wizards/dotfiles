{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "ssh.dev.azure.com vs-ssh.visualstudio.com *.visualstudio.com" = {
        identitiesOnly = true;
        identityFile = [
          "~/.ssh/id_rsa"
        ];
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
