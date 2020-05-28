pkgs:
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
    ''${pkgs.dmenu}/bin/dmenu_run -p "❤ ☮" -fn "Source Code Pro" -nb "#${dark00}" -nf "#${base05}" -sb "#${dark02}" -sf "#${base05}"'';
  dir-map = {
    j = "left";
    k = "down";
    l = "up";
    semicolon = "right";
  };
  xrandr-bullshit = dir-map // {
    k = "inverted";
    l = "normal";
  };
in
{
  enable = true;
  config = {
    fonts = [ "Monoid 8" ];
    modifier = mod;
    # menu = dmenu-run;
    focus = { mouseWarping = false; };
    keybindings = with lib.attrsets; let
      focus-keybinds = mapAttrs' (key: dir: nameValuePair "${mod}+${key}" "focus ${dir}") dir-map;
      move-keybinds = mapAttrs' (key: dir: nameValuePair "${mod}+Shift+${key}" "move ${dir}") dir-map;
      rotate-keybinds = mapAttrs' (key: dir: nameValuePair "${mod}+Ctrl+${key}" "exec xrandr -o ${dir}") xrandr-bullshit;
    in lib.mkOptionDefault (focus-keybinds // move-keybinds // rotate-keybinds);
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
