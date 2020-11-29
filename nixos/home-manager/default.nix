{ pkgs
, ...
}:
with builtins;
{
  imports = [
    ./theme.nix
    ./window-manager.nix
    ./i3status.nix

    ./zsh.nix

    ./firefox.nix
    ./obs-studio.nix

    ./vim.nix
    ./emacs.nix

    ./calibre.nix

    ./git.nix

    ./xresources.nix
  ];

  window-manager.startupPrograms = with pkgs; [
    "${pkgs.mako}/bin/mako"
    "${pkgs.firefox}/bin/firefox"

    "${pkgs.write_stylus}/bin/Write"

    "${pkgs.onboard}/bin/onboard"
    "${pkgs.pasystray}/bin/pasystray"
    "${pkgs.blueman}/bin/blueman-applet"
    "${pkgs.dropbox}/bin/dropbox"

    "${pkgs.rot8}/bin/rot8"
  ];


  programs = {
    mako.enable = true;
  };

  xdg = {
    mimeApps = {
      enable = true;
      associations.added = {
        "application/pdf" = [ "mupdf" ];
      };
      defaultApplications = {
        "application/pdf" = [ "firefox.desktop" ];
      };
    };
  };

  services = {

    udiskie = {
      enable = true;
      tray = "always";
    };

    network-manager-applet.enable = true;
    gpg-agent.enable = true;
  };

  home = {
    packages = with pkgs; [
      # Ah yes, I am doing things
      thunderbird

      # Chat with some folks
      zoom-us discord element-desktop slack

      # Reading some mafths
      mupdf
      # calibre

      # Write some mafths
      write_stylus dia

      # Plug 'n play
      udiskie

      # I got the bluez
      blueman

      # Turn that dial to 11
      pasystray pulsemixer

      # Back that shit up
      dropbox

      # Terminal stuff
      rxvt-unicode tmux

      # Misc utilites
      ranger direnv thefuck tree less jq
      htop pciutils

      # Gaaaaaaaaames (HMU if you play Celeste)
      steam

      # 12k skips / hour = 3.3 skips / second
      # Impressive if wasn't grating to my ears.
      spotify

      # *Stands on table*
      # OCaml, my caml
      ocaml dune

      # A window into windows
      virt-viewer

      # Tell me how it is
      libnotify
    ];

    sessionVariables = {
      PAGER = "less";
      EDITOR = "em";
    };

    file = {
      ".tmux.conf".text = ''
        set-window-option -g mode-keys vi
      '';
      ".profile".source = ./.profile;
      ".zprofile".source = ./.profile;
    };
    stateVersion = "20.03";
  };
}
