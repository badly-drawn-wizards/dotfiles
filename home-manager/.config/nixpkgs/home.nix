{ pkgs ? import <nixpkgs>, config, lib, ... }:
let
  i3-config = import ./i3.nix { inherit pkgs; };
  doom-emacs = pkgs.callPackage 
    (builtins.fetchTarball 
      { 
        url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
      }) 
    { doomPrivateDir = ./doom; };
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
      
      # TODO find out why picom option doesn't work
      picom &
    '';
    windowManager.i3 = i3-config;
  };

  wayland = {
    windowManager.sway = i3-config;
  };

  programs = {
    home-manager = {
      enable = true;
      path = "$HOME/dotfiles/home-manager/home-manager";
    };
    zsh = import ./zsh.nix { inherit pkgs; };
    i3status = import ./i3status.nix;
  };
  
  xresources = import ./xresources.nix;

  services = {
    dunst = import ./dunst.nix { inherit pkgs; };
    udiskie.enable = true;
    pasystray.enable = true;
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
    gpg-agent.enable = true;
    random-background = { 
      enable = true;
      imageDirectory = "%h/backgrounds";
    };
  };

  home = {
    packages = with pkgs; [
      # Lose da mouse with style
      i3-gaps
      picom

      # Browser stuff
      firefox

      # Chat with some folks
      zoom-us discord

      # Reading some mafths
      mupdf
      calibre

      # Maybe I will actually end up using these
      # xournal is very shitty on high DPI displays
      # write_stylus is better, but still shitty
      write_stylus xournal

      # Plug n' play
      udiskie

      # Charged with battery
      cbatticon

      # I got the bluez
      blueman

      # Turn that dial to 11
      pasystray pulsemixer

      # Back that shit up
      dropbox

      # A marriage made in hell
      #doom-emacs 
      vimHugeX

      # Terminal stuff
      rxvt-unicode

      # Misc utilites
      git ranger direnv thefuck tree less 
      htop pciutils

      # Gaaaaaaaaames (HMU if you play Celeste)
      # Right now steam doesn't like my laptop, but I will be back with a vengence.
      steam

      # *Stands on table* 
      # OCaml, my caml
      ocaml dune

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
      ".ideavimrc".source = ./.ideavimrc;
      ".emacs.d/init.el".text = ''
        (load "default.el")
      '';

      ".tmux.conf".text = ''
        set-window-option -g mode-keys vi 
      '';
      ".gitconfig".text = ''
        [user]
          email = reuben.steenekamp@gmail.com
          name = Reuben Steenekamp
        [push]
          default = simple
        [rebase]
          autostash = true
          autosquash = true
      '';
      ".ssh/config".text = ''
        Host github
          User git
          HostName github.com

        IdentityFile ~/.ssh/id_rsa
      '';

      
      ".zprofile".text = ''
        #!/bin/zsh
        export PATH="$HOME/.local/bin:$PATH"
      '';
      "./.local/bin/ff".text = ''
        #!/bin/sh
        exec firefox
      '';
      "./.local/bin/em".text = ''
        #!/bin/sh
        exec emacsclient -c -a "" $@
      '';

    };
    stateVersion = "20.03";
  };
}
