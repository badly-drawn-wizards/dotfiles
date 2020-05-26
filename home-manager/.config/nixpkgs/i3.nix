pkgs:
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
in
{
  enable = true;
  extraConfig = ''
  bindsym ${modifier}+j focus left
  bindsym ${modifier}+k focus down
  bindsym ${modifier}+l focus up
  bindsym ${modifier}+semicolon focus right
  bindsym ${modifier}+Shift+j move left
  bindsym ${modifier}+Shift+k move down
  bindsym ${modifier}+Shift+l move up
  bindsym ${modifier}+Shift+semicolon move right
  ''; 
  config = {
    fonts = [ "Monoid 8" ];
    modifier = modifier;
    # menu = dmenu_run;
    focus = { mouseWarping = false; };
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
