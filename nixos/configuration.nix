{ config, pkgs, ... }:

let
  sources = import ./nix/sources.nix;
in
{
  imports =
    [
      ./cachix.nix
      ./custom-hardware-configuration.nix
    ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        # From https://nixos.wiki/wiki/Accelerated_Video_Playback
        vaapiIntel = pkgs.vaapiIntel.override {
          enableHybridCodec = true;
        };
      };
    };
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
    kernelModules = [ "wl" "kvm-intel" ];
    initrd.kernelModules = [ "wl" "dm-raid" "dm-snapshot" ];

    # TODO Determine whether this actually does anything
    extraModprobeConfig = ''
      option snd_hda_intel enable=true model=laptop-amic
    '';
    
    extraModulePackages = [ ];
    cleanTmpDir = true;

    supportedFilesystems = [ "ntfs" ];
  };

  virtualisation = {
    libvirtd.enable = true;
    virtualbox.host.enable = true;
  };

  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;
    extraHosts =
      import ./spotify-sinkhole-hosts.nix;
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
    variables = {
      MESA_LOADER_DRIVER_OVERRIDE = "iris";
    };
    pathsToLink = [ "/" ];
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
    # From
    # https://nixos.wiki/wiki/Intel_Graphics 
    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
      package = (pkgs.mesa.override {
        galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
      }).drivers;
    };
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

  services = {
    lorri.enable = true;
    devmon.enable = true;
    blueman.enable = true;
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
      extraHwdb = ''
       evdev:input:b0018v04F3p29F5e0100*
        KEYBOARD_KEY_141=f12
      '';
    };
  };

  # Remember to check docs before considering changing this
  system.stateVersion = "20.03";

}
