{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        identitiesOnly = true;
        identityFile = [
          "~/.ssh/id_ed25519"
          "~/.ssh/id_rsa"
        ];
      };
      "ssh.dev.azure.com vs-ssh.visualstudio.com *.visualstudio.com" = {
        identitiesOnly = true;
        identityFile = [
          "~/.ssh/id_rsa"
          "~/.ssh/id_ed25519"
        ];
      };
      "*" = {
        identitiesOnly = true;
        identityFile = [
          "~/.ssh/id_ed25519"
          "~/.ssh/id_rsa"
        ];
      };
    };
  };
}
