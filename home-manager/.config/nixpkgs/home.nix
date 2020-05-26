{ config, pkgs, ... }:

{
  xsession = {
    enable = true;
    scriptPath = ".xinitrc";
    windowManager.i3 = import ./i3.nix pkgs;
  };

  programs = {
    zsh = import ./zsh.nix;
  };
  
  xresources = import ./xresources.nix;

  services = {
    dunst = import ./dunst.nix pkgs;
    udiskie.enable = true;
    pasystray.enable = true;
    network-manager-applet.enable = true;
    gpg-agent.enable = true;
  };

  home = {
    packages = with pkgs; [
      home-manager
      emacs
      i3
      git
      libnotify
      rxvt-unicode
      mupdf
      firefox
      calibre
      less ranger thefuck
      zoom-us discord
    ];
    sessionVariables = {
      PAGER = "less";
      EDITOR = "vim";
    };
    file = {
      ".tmux.conf".text = ''
        set-window-option -g mode-keys vi 
      '';
    };
    stateVersion = "20.03";
  };
}
