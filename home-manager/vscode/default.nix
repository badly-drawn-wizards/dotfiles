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
          "omnisharp.path" = "${pkgs.omnisharp-roslyn}/bin/OmniSharp";
          "omnisharp.loggingLevel" = "trace";
          "omnisharp.enableDecompilationSupport" = true;
          "extensions.autoCheckUpdates" = false;
          "extensions.autoUpdate" = false;
        };
      keybindings = [
        {
          key = "ctrl+o";
          command = "workbench.action.navigateBack";
        }
        {
          key = "ctrl+i";
          command = "workbench.action.navigateForward";
        }
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
        mkhl.direnv
        ms-vscode.cpptools
        # ms-python.python
      ] ++ extensionsFromVscodeMarketplace [
        {
          publisher = "ms-vscode";
          name = "makefile-tools";
          version = "0.6.0";
          sha256 = "Sd1bLdRBdLVK8y09wL/CJF+/kThPTH8MHw2mFQt+6h8=";
        }
        # {
        #   publisher = "jroesch";
        #   name = "lean";
        #   version = "0.16.56";
        #   sha256 = "BMFyuwu66xpz5D6MXOURpobKhXCU59+lzRUWuwlOjK8=";
        # }
        {
          publisher = "leanprover";
          name = "lean4";
          version = "0.0.97";
          sha256 = "uAXKN+6NWUsDV1KZ/4YjFlGy97BiuCm0NtHadpyO504=";
        }
      ];
    };

    home.packages = with pkgs; [
      omnisharp-roslyn
    ];

}
