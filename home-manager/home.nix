{ pkgs
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
    ./emacs.nix
    ./vscode
    ./calibre.nix
    ./git.nix
    ./xresources.nix
    ./networking.nix
    ./games.nix
    ./intellij.nix
    ./rot8.nix
    # ./wpaperd.nix
    # ./dictation.nix
    ./nix.nix
    ./kitty.nix
    ./gtk.nix
  ];

  window-manager.startupPrograms = with pkgs; [
    "${mako}/bin/mako"
    "${firefox}/bin/firefox"
    "${discord}"
    # "${thunderbird}/bin/thunderbird"
    # "${xournalpp}/bin/xournalpp"

    "${squeekboard}/bin/squeekboard"
    "${pasystray}/bin/pasystray"
    "${blueman}/bin/blueman-applet"
    "${dropbox}/bin/dropbox"
    # "${qbittorrent}/bin/qbittorrent"

    "${polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
  ];


  programs = {
    mako.enable = true;
    zathura = {
      enable = true;
    };
    obs-studio = {
      enable = true;
      plugins = [ pkgs.obs-studio-plugins.wlrobs ];
    };
  };

  xdg = {
    mime.enable = true;
  };

  services = {
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
      teams zoom-us discord element-desktop slack zulip franz

      squeekboard
      xfce.thunar

      godot

      # Write some mafths
      xournalpp dia

      # I got the bluez
      blueman

      # Turn that dial to 11
      pasystray pulsemixer

      # Papers please
      polkit_gnome

      # Back that shit up
      dropbox

      # Terminal stuff
      tmux

      # Misc utilites
      ranger direnv tree less jq
      htop pciutils postman lsof

      # 12k skips / hour = 3.3 skips / second
      # Impressive if wasn't grating to my ears.
      spotify

      python3
      (agda.withPackages [ agdaPackages.standard-library ])
      elan
      swiProlog

      # A window into windows
      virt-manager virt-viewer

      # Yoho yoho
      transmission-qt

      # Tell me how it is
      libnotify

      # MESA work FFS.
      libva-utils glxinfo

      texlive.combined.scheme-full

      mpv

      wl-clipboard

      unar

      docker-compose
      dbeaver
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
      ".local/bin/mpv-paste" = {
        text = ''
          #!/usr/bin/env /bin/sh
          mpv --af=scaletempo=stride=30:search=20 $(wl-paste)
        '';
        executable = true;
      };
      ".local/bin/db" = {
        text = ''
          #!/usr/bin/env /bin/sh
          GDK_BACKEND=x11 dbeaver
        '';
        executable = true;
      };
    };
    stateVersion = "20.03";
  };
}
