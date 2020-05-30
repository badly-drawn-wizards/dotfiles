{ pkgs ? import <nixpkgs> }:
with import ./spacemacs-colors.nix; 
with builtins;
let 
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
in
{
  enable = true;
  package = pkgs.i3-gaps;
  config = {
    fonts = [ "Font Awesome 8" "Fira Code 8" ];
    modifier = mod;
    menu = dmenu-run;
    focus = { mouseWarping = false; };
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
      touchscreen = "pointer:ELAN2514:00 04F3:29F5";
      rotate-script = pkgs.writeScriptBin "rotate-script" ''
        xrandr -o $1
        shift
        echo $@ | xargs xinput set-prop "${touchscreen}" 'Coordinate Transformation Matrix'
      '';
      rotate-keybinds = mapAttrs' 
        (key: dir: 
          nameValuePair "${mod}+Ctrl+${key}" ''exec ${rotate-script}/bin/rotate-script ${dir} ${getAttr dir transform-xinput}'') 
        dir-xrandr;
        
      other-keybinds = {
        "${mod}+b" = "exec random-background";
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
