lib: pkgs:
with import ./spacemacs-colors.nix; 
let 
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
  modifier = "Mod4";
  dmenu_run = 
    ''${pkgs.dmenu}/bin/dmenu_run -p "❤ ☮" -fn "Source Code Pro" -nb "#${dark00}" -nf "#${base05}" -sb "#${dark02}" -sf "#${base05}"'';
  vm_run =
    ''${pkgs.virtualbox}/bin/VirtualBoxVM -startvm Windows'';
in
{
  enable = true;
  config = {
    fonts = [ "Monoid 8" ];
    modifier = modifier;
    # menu = dmenu_run;
    focus = { mouseWarping = false; };
    keybindings = lib.mkOptionDefault {
      "${modifier}+d" = "exec ${dmenu_run}";
      "${modifier}+Shift+v" = "exec ${vm_run}";
      "${modifier}+j" = "focus left";
      "${modifier}+k" = "focus down";
      "${modifier}+l" = "focus up";
      "${modifier}+semicolon" = "focus right";
      "${modifier}+Shift+j" = "move left";
      "${modifier}+Shift+k" = "move down";
      "${modifier}+Shift+l" = "move up";
      "${modifier}+Shift+semicolon" = "move right";
    };
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
