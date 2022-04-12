{ 
pkgs, 
inputs, 
... }:


{
  imports =
    [
      inputs.dwarffs.nixosModules.dwarffs
      inputs.nix-ld.nixosModules.nix-ld
      ./theme.nix
      ./hardware
      ./power
      ./multimedia
      ./networking
      ./fonts
      ./home-manager
      ./cachix.nix
    ];

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs =  {
    # Yes, I'm an unprincipled swine. Fight me RMS.
    config = {
      allowUnfree = true;
    };
  };

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };

    libvirtd.enable = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "Africa/Johannesburg";

  environment = {
    enableDebugInfo = true;
    systemPackages = with pkgs; [ vim git ];
    pathsToLink = [ "/" ];
  };

  security.sudo = {
    enable = true;
    # TODO This doesn't take precedence, I need to investigate.
    extraRules = [
      {
        groups = [ "wheel" ];
        commands = [{ command = "ALL"; options = ["SETENV" "NOPASSWD"]; }];
      }
    ];
  };

  users = {
    groups = {
      lpadmin = {};
    };
    users.reuben = {
      isNormalUser = true;
      extraGroups = [
        "wheel" "networkmanager" "audio" "video"
        "systemd-journal" "docker" "lpadmin" "dialout"
        "wireshark"] ;
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  programs = {
    dconf.enable = true;
    sway.enable = true;
    zsh.enable = true;

    # Allow non-root network capture
    wireshark.enable = true;
  };

  services = {

    fstrim.enable = true;
    devmon.enable = true;
    blueman.enable = true;
    gvfs.enable = true;

    # TODO Get printing with Pixma G4411
    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        gutenprintBin
      ];
    };
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

    gnome.gnome-keyring.enable = true;


    dbus = {
      enable = true;
      packages = with pkgs; [ dconf ];
    };

    flatpak.enable = true;
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
      xdg-desktop-portal-gtk
    ];
    gtkUsePortal = true;
  };

  services.pipewire = {
    enable = true;
  };

  # Remember to check docs before considering changing this
  system.stateVersion = "20.03";

}
