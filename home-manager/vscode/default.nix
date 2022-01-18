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
        ms-dotnettools.csharp
      ];
    };

    home.packages = with pkgs; [
      omnisharp-roslyn
    ];

}
