{ config, pkgs, ... }:

{
  imports =
    [
      ./cachix.nix
      ./custom-hardware-configuration.nix
    ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
    kernelModules = [ "wl" ];
    initrd.kernelModules = [ "wl" "dm-raid" "dm-snapshot" ];
    
    extraModulePackages = [ ];
    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };

  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
    font = "ter-i32b";
    earlySetup = true;
  };

  fonts = {
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
      font-awesome
      emacs-all-the-icons-fonts
      source-code-pro
      noto-fonts
      noto-fonts-emoji
    ];
    enableDefaultFonts = true;
    fontconfig = {
      defaultFonts = {
        monospace = [ "Fira Code" ];
      };
    };
  };
    
  time.timeZone = "Africa/Johannesburg";

  environment = {
    systemPackages = with pkgs; [ 
      zsh
      direnv
    ];
  };

  sound.enable = true;
  hardware = {
    pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };
    bluetooth = {
      enable = true;
    };
    sensor = {
      iio.enable = true;
    };
  };

  services = {
    lorri.enable = true;
    printing.enable = true;
    devmon.enable = true;
    blueman.enable = true;
    xserver = {
      enable = true;

      videoDrivers = [ "intel" ];

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
  };

  virtualisation = {
    virtualbox.host.enable = true;
  };

  security.sudo = {
    enable = true;
    # This doesn't take precedence, I need to investigate.
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
      extraGroups = [ "wheel" "networkmanager" "audio" "video" "systemd-journal" ];
    };
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  nixpkgs.config.allowUnfree = true;

  # Remember to check docs before considering changing this
  system.stateVersion = "20.03";

}
