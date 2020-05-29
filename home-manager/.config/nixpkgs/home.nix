{ pkgs ? import <nixpkgs>, config, lib, ... }:
let
  i3-config = import ./i3.nix { inherit pkgs; };
in
{
  xsession = {
    enable = true;
    scriptPath = ".xinitrc";
    initExtra = ''
      xrdb -merge .Xresources
      udiskie &
      cbatticon &
      pasystray &
      blueman-applet &
      dropbox &
    '';
    windowManager.i3 = i3-config;
  };

  programs = {
    home-manager = {
      enable = true;
      path = "$HOME/dotfiles/home-manager/home-manager";
    };
    zsh = import ./zsh.nix { inherit pkgs; };
  };
  
  xresources = import ./xresources.nix;

  services = {
    dunst = import ./dunst.nix { inherit pkgs; };
    udiskie.enable = true;
    pasystray.enable = true;
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
    gpg-agent.enable = true;
  };

  home = {
    packages = with pkgs; [
      # Yeet
      i3

      # Browser stuff
      firefox

      # Chat with some folks
      zoom-us discord

      # Reading
      mupdf
      calibre

      # Maybe I will actually end up using these
      # xournal is very shitty on high DPI displays
      # write_stylus is better, but still shitty
      write_stylus xournal

      # Shit that belongs in the system tray 
      udiskie
      pasystray
      blueman
      cbatticon
      dropbox

      # A marriage made in hell
      emacs vim

      # Terminal stuff
      rxvt-unicode

      # Misc utilites
      git tree less ranger thefuck pulsemixer pciutils libnotify

      # Gaaaaaaaaames
      steam
    ];
    sessionVariables = {
      PAGER = "less";
      EDITOR = "vim";
    };
    file = {
      ".vimrc".text = ''
        set tabstop=2
        set shiftwidth=2
        set expandtab
        set autochdir
        set tags=./tags,tags
      '';
      ".tmux.conf".text = ''
        set-window-option -g mode-keys vi 
      '';
    };
    stateVersion = "20.03";
  };
}
