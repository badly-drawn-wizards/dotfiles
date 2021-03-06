{ pkgs, inputs, ... }:


{
  imports =
    [
      ./hardware
      ./graphics
      ./audio
      ./networking
      ./fonts
      ./overlays
      ./home-manager
      ./cachix.nix
    ];

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';
    registry.nixpkgs.flake = inputs.nixpkgs;
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
    systemPackages = with pkgs; [ 
      zsh
    ];
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
      extraGroups = [ "wheel" "networkmanager" "audio" "video" "systemd-journal" "docker" "lpadmin" "dialout" ];
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  services = {
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

    logind.extraConfig = ''
      HandlePowerKey=ignore
    '';

    dbus.enable = true;

    udev = {
      extraRules = ''
        KERNEL=="ttyUSB*", MODE="0777"
      '';
    };

    flatpak.enable = true;
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-wlr ];
  };


  # Remember to check docs before considering changing this
  system.stateVersion = "20.03";

}
