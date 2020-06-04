{ pkgs ? import <nixpkgs>
, config
, ...
}:
startup-programs:
with import ./spacemacs-colors.nix;
with builtins;
let
  cfg = config.i3;
  inherit (pkgs) lib;
  mkColorSet = bg: txt: { 
    border = "#${bg}"; 
    childBorder = "#${bg}"; 
    background = "#${bg}"; 
    indicator = "#${bg}"; 
    text = "#${txt}"; 
  };
  mkBarColorSet = bg: txt: {
    border = "#${bg}"; 
    background = "#${bg}"; 
    text = "#${txt}"; 
  };
  mod = "Mod4";

  dmenu-run = 
    ''${pkgs.dmenu}/bin/dmenu_run -p "❤ ☮" -fn "Fira Code 20" -nb "#${dark00}" -nf "#${base05}" -sb "#${dark02}" -sf "#${base05}"'';

  rofi-run = modi:
    ''${pkgs.rofi}/bin/rofi -show ${modi}'';

  rename-workspace = ''i3-input -F "rename workspace to %s"'';
  move-to-workspace = ''i3-input -F "move %s"'';

in
{
  enable = true;
  package = pkgs.i3-gaps;
  config = {
    startup = map (command: { inherit command; }) startup-programs;
    fonts = [ "Font Awesome 8" "Fira Code 8" ];
    modifier = mod;
    menu = rofi-run "run";
    focus = {
      mouseWarping = false;
      followMouse = false;
    };
    gaps = {
      inner = 5;
    };
    keybindings = with lib.attrsets; let

      # Usual i3 movement
      dir-map = {
        j = "left";
        k = "down";
        l = "up";
        semicolon = "right";
      };
      focus-keybinds = mapAttrs' (key: dir: nameValuePair "${mod}+${key}" "focus ${dir}") dir-map;
      move-keybinds = mapAttrs' (key: dir: nameValuePair "${mod}+Shift+${key}" "move ${dir}") dir-map;

      # For screen rotation with proper touch panel support adapted from
      # https://gist.github.com/Migacz85/3f544933ce5add438555ba7cd33f0413
      dir-xrandr = dir-map // {
        k = "inverted";
        l = "normal";
      };
      transform-xinput = {
        "left" = "0 -1 1 1 0 0 0 0 1";
        "right" = "0 1 0 -1 0 1 0 0 1";
        "inverted" = "-1 0 1 0 -1 1 0 0 1";
        "normal" = "1 0 0 0 1 0 0 0 1";
      };
      rotated-inputs = [
        "pointer:ELAN2514:00 04F3:29F5"
        "pointer:ELAN2514:00 04F3:29F5 Pen (0)"
      ];
      xinput-commands =
        lib.concatMapStrings
          (input: "echo $@ | xargs xinput set-prop '${input}' 'Coordinate Transformation Matrix'\n")
          rotated-inputs;
      rotate-script = pkgs.writeScriptBin "rotate-script" ''
        xrandr -o $1
        shift
        ${xinput-commands}
      '';
      rotate-keybinds = mapAttrs' 
        (key: dir: 
          nameValuePair "${mod}+Ctrl+${key}" ''exec ${rotate-script}/bin/rotate-script ${dir} ${getAttr dir transform-xinput}'') 
        dir-xrandr;
        
      other-keybinds = {
        "${mod}+Shift+-" = "move scratchpad";
        "${mod}+-" = "scratchpad show";

        "${mod}+r" = "exec ${rename-workspace}";
        "${mod}+m" = "exec ${move-to-workspace}";
        "${mod}+i" = "exec em";
        "${mod}+o" = "exec ${rofi-run "workspace"}";
        "${mod}+p" = "exec ${rofi-run "window"}";

      };

    in lib.mkOptionDefault (focus-keybinds // move-keybinds // rotate-keybinds // other-keybinds);
    colors = {
      focused = mkColorSet base02 base05;
      focusedInactive = mkColorSet base00 base0E;
      unfocused = mkColorSet base00 base0E;
    };
    bars = [{
      position = "top";
      colors = {
        background = "#${base00}";
        statusline = "#${base04}";
        separator = "#${base02}";
        focusedWorkspace = mkBarColorSet base09 dark00;    
        inactiveWorkspace = mkBarColorSet dark00 base09;    
      };
    }];
  };
}
