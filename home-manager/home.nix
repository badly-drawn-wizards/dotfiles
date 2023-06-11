{ config
, pkgs
, lib
, ...
}:
with builtins;
{
  imports = [
    ../theme.nix
    ./window-manager.nix
    ./i3status.nix
    ./zsh.nix
    ./browsers.nix
    ./obs.nix
    ./vim.nix
    ./emacs
    ./vscode
    ./calibre.nix
    ./git.nix
    ./xresources.nix
    ./networking.nix
    ./games.nix
    ./intellij.nix
    ./rot8.nix
    ./rofi.nix
    ./backgrounds.nix
    # ./wpaperd.nix
    # ./dictation.nix
    ./nix.nix
    ./kitty.nix
    ./gtk.nix
    ./osk
  ];

  windowManager.startupPrograms = with pkgs; [
    "${firefox}/bin/firefox"
    "${discord}"
    # "${thunderbird}/bin/thunderbird"
    # "${xournalpp}/bin/xournalpp"

    "${pasystray}/bin/pasystray"
    "${blueman}/bin/blueman-applet"
    "${dropbox}/bin/dropbox"
    # "${qbittorrent}/bin/qbittorrent"

    "${polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
  ];

  programs = {
    zathura = {
      enable = true;
    };
    obs-studio = {
      enable = true;
      plugins = [ pkgs.obs-studio-plugins.wlrobs ];
    };

    nix-index.enable = true;

    nix-index-database = {
      comma.enable = true;
    };
  };

  xdg = {
    mime.enable = true;
    configFile."mimeapps.list".force = true;
    mimeApps = let
      zathuraDesktop = "org.pwmt.zathura-pdf-mupdf.desktop";
      firefoxDesktop = "firefox.desktop";
      nautilusDesktop = "org.gnome.Nautilus.desktop";
      codeDesktop = "code.desktop";
      common = {
        "application/pdf" = [ zathuraDesktop ];
        "application/epub+zip" = [ zathuraDesktop ];
        "x-scheme-handler/http" = [ firefoxDesktop  ];
        "x-scheme-handler/https" = [ firefoxDesktop  ];
        "x-scheme-handler/chrome" = [ firefoxDesktop  ];
        "text/html" = [ firefoxDesktop  ];
        "application/x-extension-htm" = [ firefoxDesktop  ];
        "application/x-extension-html" = [ firefoxDesktop  ];
        "application/x-extension-shtml" = [ firefoxDesktop  ];
        "application/xhtml+xml" = [ firefoxDesktop  ];
        "application/x-extension-xhtml" = [ firefoxDesktop  ];
        "application/x-extension-xht" = [ firefoxDesktop  ];
      };
    in {
      enable = true;
      associations = {
        added = common // {
        };
        removed = {
          "inode/directory" = [ "code.desktop" ];
        };
      };
      defaultApplications = common // {
        "inode/directory" = [ nautilusDesktop ];
      };
    };
  };

  services = {
    mako.enable = true;
    udiskie = {
      enable = true;
      tray = "always";
    };

    network-manager-applet.enable = true;
    gpg-agent.enable = true;

    # Has some weird black square
    #xembed-sni-proxy.enable = true;
  };

  # xsession.pointerCursor = {
  #   name = "Vanilla-DMZ";
  #   size = 64;
  # };

  home = {
    packages = with pkgs; [
      # Ah yes, I am doing things
      thunderbird

      # Chat with some folks
      teams zoom-us discord element-desktop slack zulip franz skypeforlinux

      squeekboard
      gnome.nautilus

      godot

      xournalpp dia

      libreoffice onlyoffice-bin

      # I got the bluez
      blueman

      # Turn that dial to 11
      pasystray pulsemixer

      polkit_gnome
      glib

      # Back that shit up
      dropbox

      # Terminal stuff
      tmux

      # Misc utilites
      ranger direnv tree less jq
      btop htop postman lsof
      wget fzf

      usbutils pciutils evtest xorg.xev
      # 12k skips / hour = 3.3 skips / second
      # Impressive if wasn't grating to my ears.
      spotify

      clang-tools ccache

      (python3.withPackages (ps: with ps; [
        numpy matplotlib
        torch transformers
        ipython
        python-uinput
      ]))

      python3Packages.python-lsp-server

      # (agda.withPackages [ agdaPackages.standard-library ])

      elan

      # A window into windows
      virt-manager virt-viewer

      # Yoho yoho
      transmission-qt

      # Tell me how it is
      libnotify

      # MESA work FFS.
      libva-utils glxinfo

      # texlive.combined.scheme-full

      mpv

      wl-clipboard

      unar unrar p7zip

      docker-compose
      dbeaver

      wireshark

      zrythm
      renoise

      #nix-alien nix-index-update nix-index

      dotnet-sdk

    ];

    sessionVariables = {
      PAGER = "less";
      EDITOR = "em";
    };


    sessionPath = [
      "${config.home.homeDirectory}/.local/bin"
      "${config.home.homeDirectory}/.dotnet/tools"
    ];

    file = {
      ".config/gdb/gdbinit".text = ''
      set auto-load safe-path /
      '';
      ".tmux.conf".text = ''
        set-window-option -g mode-keys vi
      '';
      ".local/bin/mpv-paste" = {
        text = ''
          #!/usr/bin/env /bin/sh
          mpv --af=scaletempo=stride=30:search=20 $(wl-paste)
        '';
        executable = true;
      };
    };
    stateVersion = "20.03";
  };

  systemd.user.sessionVariables = config.home.sessionVariables;
}
