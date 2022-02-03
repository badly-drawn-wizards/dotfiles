{ config, lib, pkgs, ... }:

let
  inherit (pkgs.vscode-utils) extensionsFromVscodeMarketplace;
in
{
    programs.vscode = {
      enable = true;
      userSettings = with builtins;
        (fromJSON (readFile ./settings.json) //
        (fromJSON (readFile ./vspacecode/settings.json)) // {
        "omnisharp.path" = "${pkgs.omnisharp-roslyn}/bin/omnisharp";
        "omnisharp.loggingLevel" = "trace";
        "extensions.autoCheckUpdates" = false;
        "extensions.autoUpdate" = false;
      });
      keybindings = [
      ] ++ builtins.fromJSON (builtins.readFile ./vspacecode/keybindings.json);
      extensions = with pkgs.vscode-extensions; [
        bbenoist.nix
        vscodevim.vim
        kahole.magit
        vspacecode.whichkey
        vspacecode.vspacecode
        ms-dotnettools.csharp
        dracula-theme.theme-dracula
        ms-azuretools.vscode-docker
        esbenp.prettier-vscode
      ] ++ extensionsFromVscodeMarketplace [
        {
          publisher = "rubymaniac";
          name = "vscode-direnv";
          version = "0.0.2";
          sha256 = "TVvjKdKXeExpnyUh+fDPl+eSdlQzh7lt8xSfw1YgtL4=";
        }
      ];
    };

    home.packages = with pkgs; [
      omnisharp-roslyn
    ];

}
