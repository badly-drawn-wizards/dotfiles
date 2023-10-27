{
pkgs,
lib,
inputs,
... 
}:


{
  imports =
    [
      inputs.dwarffs.nixosModules.dwarffs
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

      inputs.dotfiles-private.nixosModules.dotfiles-private
      inputs.k8s-vm.nixosModules.k8s-vm
    ];

  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" "nixos-config=/workspace/dotfiles" ];
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
    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };

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
