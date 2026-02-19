{ pkgs
, lib
, inputs
, ...
}:


{
  imports =
    [
      ./theme.nix
      ./hardware
      ./power
      ./multimedia
      ./networking
      ./fonts
      ./home-manager
      ./virtualization
      ./cachix.nix
      ./ccache.nix
      ./users.nix
      ./security.nix
      ./speech.nix
    ];

  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" "nixos-config=/etc/nixos" ];
    package = pkgs.nixVersions.stable;
    settings = {
      extra-sandbox-paths = [ "/nix/var" ];
      builders-use-substitutes = true;
      experimental-features = [ "nix-command" "flakes" "recursive-nix" ];
      trusted-substituters = [ "https://cache.nixos.org/" ];
    };

    # Automatic garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    # Automatic store deduplication
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "Africa/Johannesburg";

  environment = {
    enableDebugInfo = true;
    systemPackages = with pkgs; [ vim git ];
    pathsToLink = [
      "/share"
      "/share/zsh"
      "/share/xdg-desktop-portal"
      "/share/applications"
    ];
  };

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };

    dconf.enable = true;

    # Allow non-root network capture
    wireshark.enable = true;

    # systemtap.enable = true;
    nix-ld.enable = true;
  };

  services = {
    # openssh.enable = true;

    acpid.enable = true;
    asusd.enable = true;

    fstrim.enable = true;
    devmon.enable = true;
    gvfs.enable = true;

    xserver = {
      enable = true;
      displayManager.startx.enable = true;
      videoDrivers = [ "modesetting" "amdgpu" ];
    };

    gnome = {
      gnome-settings-daemon.enable = true;
      at-spi2-core.enable = true;
    };

    dbus = {
      enable = true;
      packages = with pkgs; [ dconf gcr ];
    };

    flatpak.enable = true;

    # fprintd.enable = true;

    fwupd.enable = true;

    wyoming.piper.servers.piper = {
      enable = true;
      uri = "tcp://0.0.0.0:10200";
      voice = "en-us-ryan-medium";
    };

  };


  # services.xserver.desktopManager.plasma5.enable = true;
  # environment.plasma5 = {
  #   excludePackages = with pkgs.libsForQt5; [
  #     elisa
  #     gwenview
  #     okular
  #     oxygen
  #     khelpcenter
  #     konsole
  #     plasma-browser-integration
  #     print-manager
  #   ];
  # };

  xdg = {
    mime.enable = true;
    icons.enable = true;
    portal = {
      enable = true;
      wlr.enable = true;
      xdgOpenUsePortal = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
      config.common.default = [ "wlr" "gtk" ];
    };
  };

  system.stateVersion = "23.11";
}
