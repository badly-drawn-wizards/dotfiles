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

      # To deploy elsewhere later
      ./home-assistant.nix

      inputs.dwarffs.nixosModules.dwarffs
      inputs.microvm.nixosModules.host
      inputs.dotfiles-private.nixosModules.dotfiles-private
    ];

  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" "nixos-config=/workspace/dotfiles" ];
    package = pkgs.nixVersions.stable;
    settings = {
      extra-sandbox-paths = [ "/nix/var" ];
      builders-use-substitutes = true;
      experimental-features = [ "nix-command" "flakes" "repl-flake" "recursive-nix" ];
      trusted-substituters = [ "https://cache.nixos.org/" ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "Africa/Johannesburg";

  environment = {
    enableDebugInfo = true;
    systemPackages = with pkgs; [ vim git ];
    pathsToLink = [ "/share" "/share/zsh" ];
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
    openssh.enable = true;

    acpid.enable = true;
    asusd.enable = true;
    ratbagd.enable = true;

    # Somehow permissions are fucked, will figure out later
    # clamav = {
    #   daemon.enable = true;
    #   updater.enable = true;
    # };

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

    fprintd.enable = true;

    fwupd.enable = true;

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
      config.common.default = "*";
      xdgOpenUsePortal = false;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
  };

  system.stateVersion = "23.11";

}
