{ config, lib, pkgs, ... }:

let
  inherit (lib) escapeShellArg attrNames concatStrings;
in
{
  options = {
    programs.rofi = with lib; with types; {
      modi = mkOption {
        type = attrsOf (nullOr (either str path));
        default = { };
      };
      cmd = {
        base = mkOption {
          type = str;
          default = ''${config.programs.rofi.finalPackage}/bin/rofi -matching fuzzy'';
        };
        dmenu = mkOption {
          type = functionTo str;
          default = prompt: ''${config.programs.rofi.cmd.base} -dmenu -p ${escapeShellArg prompt}'';
        };
        modi = mkOption {
          type = functionTo str;
          default = modi: ''${config.programs.rofi.cmd.base} -show ${escapeShellArg modi}'';
        };
      };
    };
  };

  config = {
    programs.rofi = {
      enable = true;
      theme = "${pkgs.dracula-rofi-theme}/theme/config2.rasi";
      package = pkgs.rofi;
      font = "{config.font} ${builtins.toString config.fontSize}";
      extraConfig = {
        modi = builtins.concatStringsSep "," (lib.mapAttrsToList
          (name: script:
            name + (if isNull script then "" else ":${script}"))
          config.programs.rofi.modi);
        kb-row-tab = "";
        kb-remove-to-eol = "";
        kb-element-next = "";
        kb-element-prev = "";
        kb-accept-entry = "Return";
        kb-mode-next = "Tab";
        kb-mode-previous = "Shift+Tab";
        kb-row-up = "Control+k";
        kb-row-down = "Control+j";
      };
    };
  };
}
