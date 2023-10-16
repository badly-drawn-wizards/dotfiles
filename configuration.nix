{
pkgs,
lib,
inputs,
... }:


{
  imports =
    [
      inputs.dwarffs.nixosModules.dwarffs
      # inputs.unhinged.nixosModules.unhinged
      ./theme.nix
      ./hardware
      ./power
      ./multimedia
      ./networking
      ./fonts
      ./home-manager
      ./cachix.nix
      ./ccache.nix
      ./virtualization.nix
      ./users.nix
      ./security.nix
    ];

  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes repl-flake
    '';
  };

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "Africa/Johannesburg";

  environment = {
    enableDebugInfo = true;
    systemPackages = with pkgs; [ vim git ];
    pathsToLink = [ "/share" ];
  };

  programs = {
    dconf.enable = true;
    zsh.enable = true;

    # Allow non-root network capture
    wireshark.enable = true;

    # systemtap.enable = true;
    nix-ld.enable = true;
  };

  services = {

    fstrim.enable = true;
    devmon.enable = true;
    gvfs.enable = true;

    avahi = {
      enable = true;
      nssmdns = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };

    xserver = {
      enable = true;
      displayManager.startx.enable = true;
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

    nixfs.enable = false;
  };

  xdg = {
    mime.enable = true;
    icons.enable = true;
    portal = {
      enable = true;
      xdgOpenUsePortal = false;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
  };

  system.stateVersion = "23.11";

}
