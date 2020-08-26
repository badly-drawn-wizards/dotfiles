{ config, pkgs, ... }:

let
  sources = import /workspace/dotfiles/nix/sources.nix;
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
    ];

  nix.nixPath = [
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    "nixos-config=/etc/nixos/configuration.nix"
    "nixpkgs-overlays=/workspace/dotfiles/overlays"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];

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
    users.reuben = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "audio" "video" "systemd-journal" "docker" ];
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  services = {
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
    };

    logind.extraConfig = ''
      HandlePowerKey=ignore
    '';

    dbus.enable = true;

  };

  # Remember to check docs before considering changing this
  system.stateVersion = "20.03";

}
