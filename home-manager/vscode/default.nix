{ config, lib, pkgs, ... }:

let
  inherit (builtins) fromJSON readFile map filter;
  readJSON = path: fromJSON (readFile path);
  settings = readJSON ./settings.json;
  defaultKeybindings = readJSON ./default/keybindings.json;
  excludeDefaultKeybinding = key:
    map (binding: binding // { command = "-${binding.command}"; })
      (filter (binding: binding.key == key) defaultKeybindings);
  vspacecodeSettings = readJSON ./vspacecode/settings.json;
  vspacecodeKeybindings = readJSON ./vspacecode/keybindings.json;
  whichkeyOverrides = readJSON ./whichkey.json;
  inherit (pkgs.vscode-utils) extensionsFromVscodeMarketplace;
in
{
    programs.vscode = {
      enable = true;
      userSettings =
        settings //
        vspacecodeSettings //
        {
          "whichkey.bindingOverrides" = whichkeyOverrides;
          "omnisharp.path" = "${pkgs.omnisharp-roslyn}/bin/omnisharp";
          "omnisharp.loggingLevel" = "trace";
          "omnisharp.enableDecompilationSupport" = true;
          "extensions.autoCheckUpdates" = false;
          "extensions.autoUpdate" = false;
        };
      keybindings = [
      ] ++ excludeDefaultKeybinding "ctrl+o" ++ vspacecodeKeybindings;
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
        angular.ng-template
      ] ++ extensionsFromVscodeMarketplace [
        # {
        #   publisher = "rubymaniac";
        #   name = "vscode-direnv";
        #   version = "0.0.2";
        #   sha256 = "TVvjKdKXeExpnyUh+fDPl+eSdlQzh7lt8xSfw1YgtL4=";
        # }
        {
          publisher = "mkhl";
          name = "direnv";
          version = "0.5.0";
          sha256 = "kkR+f92GEQM2FLEgeBlTa5W1w1IV+umxRg+kVlWh+8s=";
        }
        {
          publisher = "jroesch";
          name = "lean";
          version = "0.16.45";
          sha256 = "bZJ374kvzZpjxTBbadHLEoET7ilRhu4afuWN+qG0Tng=";
        }
        {
          publisher = "genuitec";
          name = "angular-cli-task-provider";
          version = "1.3.2";
          sha256 = "dy4zcc7fgtaOGNlTbUGXsth/cEadx8ql1BE7IxyrAjI=";
        }
        # {
        #   publisher = "redhat";
        #   name = "fabric8-analytics";
        #   version = "0.3.5";
        #   sha256 = "D96ADlqjQMhXLaVLTHmpPrIqgy1FGt0QZTPBo90DF30=";
        # }
      ];
    };

    home.packages = with pkgs; [
      omnisharp-roslyn
    ];

}
