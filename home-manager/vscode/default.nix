{ config, lib, pkgs, ... }:

let
  inherit (builtins) fromJSON readFile map filter;
  readJSON = path: fromJSON (readFile path);
  settings = readJSON ./settings.json;
  defaultKeybindings = readJSON ./default/keybindings.json;
  excludeDefaultKeybinding = key:
    map (binding: binding // { command = "-${binding.command}"; })
      (filter (binding: binding.key == key) defaultKeybindings);
  defaultCommandsToSkipShellToRemove = [
    # "editor.action.toggleTabFocusMode"
    # "notifications.hideList"
    # "notifications.hideToasts"
    # "workbench.action.closeQuickOpen"
    # "workbench.action.debug.continue"
    # "workbench.action.debug.pause"
    # "workbench.action.debug.restart"
    # "workbench.action.debug.run"
    # "workbench.action.debug.start"
    # "workbench.action.debug.stepInto"
    # "workbench.action.debug.stepOut"
    # "workbench.action.debug.stepOver"
    # "workbench.action.debug.stop"
    # "workbench.action.firstEditorInGroup"
    # "workbench.action.focusActiveEditorGroup"
    # "workbench.action.focusEighthEditorGroup"
    # "workbench.action.focusFifthEditorGroup"
    # "workbench.action.focusFirstEditorGroup"
    # "workbench.action.focusFourthEditorGroup"
    # "workbench.action.focusLastEditorGroup"
    # "workbench.action.focusNextPart"
    # "workbench.action.focusPreviousPart"
    # "workbench.action.focusSecondEditorGroup"
    # "workbench.action.focusSeventhEditorGroup"
    # "workbench.action.focusSixthEditorGroup"
    # "workbench.action.focusThirdEditorGroup"
    # "workbench.action.lastEditorInGroup"
    # "workbench.action.navigateDown"
    # "workbench.action.navigateLeft"
    # "workbench.action.navigateRight"
    # "workbench.action.navigateUp"
    # "workbench.action.nextEditor"
    # "workbench.action.nextEditorInGroup"
    # "workbench.action.nextPanelView"
    # "workbench.action.nextSideBarView"
    # "workbench.action.openNextRecentlyUsedEditor"
    # "workbench.action.openNextRecentlyUsedEditorInGroup"
    # "workbench.action.openPreviousRecentlyUsedEditor"
    # "workbench.action.openPreviousRecentlyUsedEditorInGroup"
    # "workbench.action.previousEditor"
    # "workbench.action.previousEditorInGroup"
    # "workbench.action.previousPanelView"
    # "workbench.action.previousSideBarView"
    "workbench.action.quickOpen"
    "workbench.action.quickOpenLeastRecentlyUsedEditor"
    "workbench.action.quickOpenLeastRecentlyUsedEditorInGroup"
    "workbench.action.quickOpenPreviousEditor"
    "workbench.action.quickOpenPreviousRecentlyUsedEditor"
    "workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup"
    "workbench.action.quickOpenView"
    "workbench.action.showCommands"
    # "workbench.action.tasks.build"
    # "workbench.action.tasks.reRunTask"
    # "workbench.action.tasks.restartTask"
    # "workbench.action.tasks.runTask"
    # "workbench.action.tasks.showLog"
    # "workbench.action.tasks.showTasks"
    # "workbench.action.tasks.terminate"
    # "workbench.action.tasks.test"
    # "workbench.action.terminal.acceptSelectedSuggestion"
    # "workbench.action.terminal.clear"
    # "workbench.action.terminal.clearSelection"
    # "workbench.action.terminal.copyAndClearSelection"
    # "workbench.action.terminal.copyLastCommandOutput"
    # "workbench.action.terminal.copySelection"
    # "workbench.action.terminal.copySelectionAsHtml"
    # "workbench.action.terminal.deleteToLineStart"
    # "workbench.action.terminal.deleteWordLeft"
    # "workbench.action.terminal.deleteWordRight"
    # "workbench.action.terminal.findNext"
    # "workbench.action.terminal.findPrevious"
    # "workbench.action.terminal.focus"
    # "workbench.action.terminal.focusAtIndex1"
    # "workbench.action.terminal.focusAtIndex2"
    # "workbench.action.terminal.focusAtIndex3"
    # "workbench.action.terminal.focusAtIndex4"
    # "workbench.action.terminal.focusAtIndex5"
    # "workbench.action.terminal.focusAtIndex6"
    # "workbench.action.terminal.focusAtIndex7"
    # "workbench.action.terminal.focusAtIndex8"
    # "workbench.action.terminal.focusAtIndex9"
    # "workbench.action.terminal.focusFind"
    # "workbench.action.terminal.focusNext"
    # "workbench.action.terminal.focusNextPane"
    # "workbench.action.terminal.focusPrevious"
    # "workbench.action.terminal.focusPreviousPane"
    # "workbench.action.terminal.goToRecentDirectory"
    # "workbench.action.terminal.hideFind"
    # "workbench.action.terminal.hideSuggestWidget"
    # "workbench.action.terminal.kill"
    # "workbench.action.terminal.killEditor"
    # "workbench.action.terminal.moveToEditor"
    # "workbench.action.terminal.moveToLineEnd"
    # "workbench.action.terminal.moveToLineStart"
    # "workbench.action.terminal.moveToTerminalPanel"
    # "workbench.action.terminal.navigationModeExit"
    # "workbench.action.terminal.navigationModeFocusNext"
    # "workbench.action.terminal.navigationModeFocusPrevious"
    # "workbench.action.terminal.new"
    # "workbench.action.terminal.newInActiveWorkspace"
    # "workbench.action.terminal.paste"
    # "workbench.action.terminal.pasteSelection"
    # "workbench.action.terminal.quickFix"
    # "workbench.action.terminal.resizePaneDown"
    # "workbench.action.terminal.resizePaneLeft"
    # "workbench.action.terminal.resizePaneRight"
    # "workbench.action.terminal.resizePaneUp"
    # "workbench.action.terminal.runActiveFile"
    # "workbench.action.terminal.runRecentCommand"
    # "workbench.action.terminal.runSelectedText"
    # "workbench.action.terminal.scrollDown"
    # "workbench.action.terminal.scrollDownPage"
    # "workbench.action.terminal.scrollToBottom"
    # "workbench.action.terminal.scrollToNextCommand"
    # "workbench.action.terminal.scrollToPreviousCommand"
    # "workbench.action.terminal.scrollToTop"
    # "workbench.action.terminal.scrollUp"
    # "workbench.action.terminal.scrollUpPage"
    # "workbench.action.terminal.selectAll"
    # "workbench.action.terminal.selectNextPageSuggestion"
    # "workbench.action.terminal.selectNextSuggestion"
    # "workbench.action.terminal.selectPrevPageSuggestion"
    # "workbench.action.terminal.selectPrevSuggestion"
    # "workbench.action.terminal.selectToNextCommand"
    # "workbench.action.terminal.selectToNextLine"
    # "workbench.action.terminal.selectToPreviousCommand"
    # "workbench.action.terminal.selectToPreviousLine"
    # "workbench.action.terminal.sendSequence"
    # "workbench.action.terminal.showAccessibilityHelp"
    # "workbench.action.terminal.sizeToContentWidth"
    # "workbench.action.terminal.split"
    # "workbench.action.terminal.splitInActiveWorkspace"
    # "workbench.action.terminal.toggleFindCaseSensitive"
    # "workbench.action.terminal.toggleFindRegex"
    # "workbench.action.terminal.toggleFindWholeWord"
    # "workbench.action.terminal.toggleTerminal"
    # "workbench.action.toggleFullScreen"
    # "workbench.action.toggleMaximizedPanel"
    "workbench.action.togglePanel"
  ];
  commandsToSkipShell = map (binding: "-${binding}") defaultCommandsToSkipShellToRemove;
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
          # "omnisharp.path" = "${pkgs.omnisharp-roslyn}/bin/OmniSharp";
          "omnisharp.loggingLevel" = "trace";
          "omnisharp.enableDecompilationSupport" = true;
          "extensions.autoCheckUpdates" = false;
          "extensions.autoUpdate" = false;
          "terminal.integrated.commandsToSkipShell" = commandsToSkipShell;
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
      ]
      ++ excludeDefaultKeybinding "ctrl+o"
      ++ excludeDefaultKeybinding "ctrl+j"
      ++ vspacecodeKeybindings;
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
