{ pkgs
, config
, lib
, ...
}@args:
with builtins;
let
  startup-programs = with pkgs; [

    "${pkgs.firefox}/bin/firefox"

    "${pkgs.calibre}/bin/calibre"

    "${pkgs.write_stylus}/bin/Write"

    "${pkgs.riot-desktop}/bin/riot-desktop"
    "${pkgs.discord}/bin/Discord"
    "${pkgs.slack}/bin/slack"

    "${pkgs.pasystray}/bin/pasystray"
    "${pkgs.blueman}/bin/blueman-applet"
    "${pkgs.dropbox}/bin/dropbox"
  ];
  i3-config = import ./i3.nix args startup-programs;
  load-xresources = "xrdb -merge .Xresources";
in
{

  xsession = {
    enable = true;
    scriptPath = ".xinitrc";
    initExtra = load-xresources;
    windowManager.i3 = i3-config "i3";
  };

  wayland = {
    windowManager.sway = i3-config "sway";
  };

  programs = {
    home-manager = {
      enable = true;
      #path = sources.home-manager.outPath;
    };

    firefox = import ./firefox.nix;

    zsh = import ./zsh.nix args;
    rofi = import ./rofi.nix args;

    i3status = import ./i3status.nix;
  };
  
  xresources = import ./xresources.nix;

  services = {
    # Give me those notifications
    dunst = import ./dunst.nix args;

    # Transparent windows and free of artifacts
    picom.enable = true;

    # Screensavers so interesting they
    # make me not want to use my laptop
    xscreensaver = {
      enable = true;
    };

    udiskie = {
      enable = true;
      tray = "always";
    };

    network-manager-applet.enable = true;
    gpg-agent.enable = true;
    random-background = { 
      enable = true;
      
      imageDirectory = "%h/backgrounds";
      interval = "20m";
    };
  };

  nixpkgs = {
    overlays = [
      (_:_: {
      doom-emacs = pkgs.callPackage
        (builtins.fetchTarball
          {
            url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
          })
        {
          doomPrivateDir = ./doom;
          extraPackages = epkgs: [ epkgs.doom-themes ];
        };
      })
    ];

    config = {
      # Yes, I am an unprincipled swine. Fight me RMS.
      allowUnfree = true;

      firefox = {
        enableTridactylNative = true;
        ffmpegSupport = true;
      };
    };
  };

  home = {
    packages = with pkgs; [
      # Information super highway
      firefox

      # Lose da mouse with style
      i3-gaps dmenu
      picom

      # Chat with some folks
      zoom-us discord riot-desktop slack

      # Reading some mafths
      mupdf
      calibre

      # Write some mafths
      write_stylus xournal dia

      # Plug 'n play
      udiskie

      # I got the bluez
      blueman

      # Turn that dial to 11
      pasystray pulsemixer

      # Back that shit up
      dropbox

      # A marriage made in hell
      vimHugeX

      # Terminal stuff
      rxvt-unicode tmux

      # Misc utilites
      git ranger direnv thefuck tree less jq
      htop pciutils

      # Gaaaaaaaaames (HMU if you play Celeste)
      steam

      # 12k skips / hour = 3.5 skips / second
      # Impressive if wasn't grating to my ears.
      spotify

      # *Stands on table* 
      # OCaml, my caml
      ocaml dune

      # A window into windows
      virt-viewer

      # Streaming my life
      obs-studio

      # Tell me how it is
      espeak
    ];

    sessionVariables = {
      PAGER = "less";
      EDITOR = "em";
      MOZ_USE_XINPUT2 = "1";
      CALIBRE_USE_DARK_PALETTE = "1";
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

      ".profile".source = ./.profile;
      ".zprofile".source = ./.profile;
      "./.local/bin/random-background".source = ./bin/random-background;

      "./.local/bin/em".source =
        let
          em-path =
            pkgs.writeScriptBin "em" ''
              #!/bin/sh
              ${pkgs.emacs}/bin/emacsclient -c -a "" $@
            '';
        in
        "${em-path}/bin/em";

      ".xscreensaver".source = ./.xscreensaver;
    };
    stateVersion = "20.03";
  };
}
