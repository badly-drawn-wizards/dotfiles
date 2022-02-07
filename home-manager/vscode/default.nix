{ config, lib, pkgs, ... }:

let
  inherit (pkgs.vscode-utils) extensionsFromVscodeMarketplace;
in
{
    programs.vscode = {
      enable = true;
      userSettings = with builtins;
        (fromJSON (readFile ./settings.json) //
        (fromJSON (readFile ./vspacecode/settings.json)) //
        {
          "whichkey.bindingOverrides" = (fromJSON (readFile ./whichkey.json));
        } // {
          "omnisharp.path" = "${pkgs.omnisharp-roslyn}/bin/omnisharp";
          "omnisharp.loggingLevel" = "trace";
          "omnisharp.enableDecompilationSupport" = true;
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
        {
          publisher = "jroesch";
          name = "lean";
          version = "0.16.45";
          sha256 = "sha256-bZJ374kvzZpjxTBbadHLEoET7ilRhu4afuWN+qG0Tng=";
        }
      ];
    };

    home.packages = with pkgs; [
      omnisharp-roslyn
    ];

}
