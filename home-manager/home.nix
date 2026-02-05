{ config
, pkgs
, lib
, ...
}:
with builtins;
{
  imports = [
    ../theme.nix
    ./theme.nix
    ./window-manager
    ./browsers
    ./tray.nix
    ./editors
    ./mail.nix
    ./media
    ./messaging.nix
    ./zsh.nix
    ./git.nix
    ./xresources.nix
    ./screen-rotation.nix
    ./backgrounds.nix
    ./nix.nix
    ./kitty.nix
    ./gtk.nix
    ./osk
    ./sync.nix
    ./virtualization.nix
    ./lang
    # ./tts.nix
    ./gpg.nix
    ./security.nix
    ./image-editor.nix
    # ./pomo
    ./task-management
  ];

  manual = {
    html.enable = true;
    manpages.enable = true;
    json.enable = true;
  };

  programs = {
    zathura.enable = true;
  };

  xdg = {
    mime.enable = true;
    configFile."mimeapps.list".force = true;
    mimeApps =
      let
        zathuraDesktop = "org.pwmt.zathura-pdf-mupdf.desktop";
        firefoxDesktop = "firefox.desktop";
        nautilusDesktop = "org.gnome.Nautilus.desktop";
        codeDesktop = "code.desktop";
        common = {
          "application/pdf" = [ zathuraDesktop ];
          "application/epub+zip" = [ zathuraDesktop ];
          "x-scheme-handler/http" = [ firefoxDesktop ];
          "x-scheme-handler/https" = [ firefoxDesktop ];
          "x-scheme-handler/chrome" = [ firefoxDesktop ];
          "text/html" = [ firefoxDesktop ];
          "application/x-extension-htm" = [ firefoxDesktop ];
          "application/x-extension-html" = [ firefoxDesktop ];
          "application/x-extension-shtml" = [ firefoxDesktop ];
          "application/xhtml+xml" = [ firefoxDesktop ];
          "application/x-extension-xhtml" = [ firefoxDesktop ];
          "application/x-extension-xht" = [ firefoxDesktop ];
          "application/x-www-browser" = [ firefoxDesktop ];
        };
      in
      {
        enable = true;
        associations = {
          added = common // { };
          removed = {
            "inode/directory" = [ "code.desktop" ];
          };
        };
        defaultApplications = common // {
          "inode/directory" = [ nautilusDesktop ];
        };
      };
  };


  home = {
    packages = with pkgs; [
      xournalpp

      libreoffice

      pulsemixer

      tmux

      tree
      less
      jq
      btop
      lsof
      wget
      fzf

      transmission_4-qt

      wl-clipboard

      unar
      unrar
      p7zip

      pciutils
      usbutils
      iw
      nettools
      wirelesstools
      d-spy

      asusctl

      screenshot
      feh

      # webnovel-android
      github-desktop

      nettools

      # cura

      burpsuite

      claude-code
    ];

    sessionVariables = {
      PAGER = "less";
      EDITOR = "reloadable-nvim";
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
    };
    stateVersion = "20.03";
  };

  systemd.user.sessionVariables = config.home.sessionVariables;
}
