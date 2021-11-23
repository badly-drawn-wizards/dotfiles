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
    ./browsers.nix
    ./obs-studio.nix
    ./vim.nix
    ./emacs.nix
    ./calibre.nix
    ./git.nix
    ./xresources.nix
    ./networking.nix
    ./games.nix
    ./intellij.nix
  ];

  window-manager.startupPrograms = with pkgs; [
    "${mako}/bin/mako"
    "${firefox}/bin/firefox"
    "${thunderbird}/bin/thunderbird"
    "${xournalpp}/bin/xournalpp"

    "${onboard}/bin/onboard"
    "${pasystray}/bin/pasystray"
    "${blueman}/bin/blueman-applet"
    "${dropbox}/bin/dropbox"

    "${rot8}/bin/rot8"
    "${polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
  ];


  programs = {
    mako.enable = true;
    vscode = {
      enable = true;
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
  };

  home = {
    packages = with pkgs; [
      # Ah yes, I am doing things
      thunderbird

      # Chat with some folks
      teams zoom-us discord element-desktop slack zulip

      # Reading some mafths
      mupdf

      # Write some mafths
      xournalpp dia

      # Plug 'n play
      udiskie

      # I got the bluez
      blueman

      # Turn that dial to 11
      pasystray pulsemixer

      # Papers please
      polkit_gnome

      # Back that shit up
      dropbox

      # Terminal stuff
      rxvt-unicode tmux

      # Misc utilites
      ranger direnv tree less jq
      htop pciutils postman

      # 12k skips / hour = 3.3 skips / second
      # Impressive if wasn't grating to my ears.
      spotify

      python3
      (agda.withPackages [ agdaPackages.standard-library ])
      elan

      # A window into windows
      virt-manager virt-viewer wineWowPackages.staging winetricks

      # Tell me how it is
      libnotify

      # MESA work FFS.
      libva-utils glxinfo

      # Nix tools
      nix-prefetch-github
      cachix

      gnome3.adwaita-icon-theme

      texlive.combined.scheme-full

      mpv

      wl-clipboard
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
    };
    stateVersion = "20.03";
  };
}
