{ config, pkgs, ... }:

let
  sources = import ./nix/sources.nix;
  home-manager = import ./home-manager;
in
{
  imports =
    [
      ./cachix.nix
      ./hardware
      ./graphics
      ./audio
      ./networking
      ./fonts
      "${sources.home-manager}/nixos"
    ];

  nixpkgs =  {
    overlays = [
      (_:_: {
        niv-pkgs = import sources.nixpkgs {};
      })
    ];

    # Yes, I'm an unprincipled swine. Fight me RMS.
    config.allowUnfree = true;
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
    users.reuben = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "audio" "video" "systemd-journal" "docker" ];
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };
  home-manager.users.reuben = home-manager;

  services = {
    lorri.enable = true;
    devmon.enable = true;
    blueman.enable = true;

    # TODO Get printing with Pixma G4411
    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        gutenprintBin
      ];
    };

    xserver = {
      enable = true;

      displayManager.startx.enable = true;

      layout = "us";
      xkbOptions = "caps:swapescape";
      libinput.enable = true;

    };

    logind.extraConfig = ''
      HandlePowerKey=ignore
    '';

    dbus = {
      enable = true;
      packages = with pkgs; [ blueman ];
    };

    udev = {
      # TODO Get HP Active Pen rubber button working
      # extraHwdb = ''
      #  evdev:input:b0018v04F3p29F5e0100*
      #   KEYBOARD_KEY_141=f12
      # '';
    };
  };

  # Remember to check docs before considering changing this
  system.stateVersion = "20.03";

}
