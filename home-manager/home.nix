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
    ./gpg.nix
    ./security.nix
    ./image-editor.nix
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
  };


  home = {
    packages = with pkgs; [
      xournalpp

      libreoffice

      pulsemixer

      nautilus

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
      wdisplays

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

      devenv

      pied
    ];

    sessionVariables = {
      PAGER = "less";
      EDITOR = "nvim";
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
