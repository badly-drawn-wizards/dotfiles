{ config, lib, pkgs, ... }:

{
    programs.vscode = {
      enable = true;
      userSettings = with builtins; (fromJSON (readFile ./settings.json) // {
        "omnisharp.path" = "${pkgs.omnisharp-roslyn}/bin/omnisharp";
        "extensions.autoCheckUpdates" = false;
        "extensions.autoUpdate" = false;
      });
      extensions = with pkgs.vscode-extensions; [
        bbenoist.nix
        vscodevim.vim
        kahole.magit
        vspacecode.whichkey
        vspacecode.vspacecode
        ms-dotnettools.csharp
        dracula-theme.theme-dracula
        ms-azuretools.vscode-docker
      ];
    };

    home.packages = with pkgs; [
      omnisharp-roslyn
    ];

}
