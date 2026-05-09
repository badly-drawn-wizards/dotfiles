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

    evolution = {
      enable = true;
      plugins = [ pkgs.evolution-ews ];
    };

    azurevpnclient.enable = true;
    azurevpnclient.package = pkgs.microsoft-azurevpnclient;
  };

  # Populate /etc/ssl/certs/ with cacert.hashed contents:
  #   - <subjectHash>.N symlinks (X509_LOOKUP_hash_dir; PR #370023)
  #   - per-cert .crt files (TRUSTED CERTIFICATE / X509_AUX format)
  # PLUS plain-PEM .pem aliases (each .crt run through `openssl x509`
  # to strip the X509_AUX trust extensions and human-readable "Trusted
  # for:" annotations). cacert ships X509_AUX format, but the Dart
  # parser in AzureVPNClient only handles plain "BEGIN CERTIFICATE"
  # PEM and FormatException's on TRUSTED CERTIFICATE blocks.
  # https://discourse.nixos.org/t/help-getting-azure-vpn-to-work/60309/2
  environment.etc =
    let
      hashedCertsDir = "${pkgs.cacert.hashed}/etc/ssl/certs";
      hashedContents = builtins.readDir hashedCertsDir;
      hashedEntries = lib.mapAttrs'
        (name: _: lib.nameValuePair "ssl/certs/${name}" {
          source = "${hashedCertsDir}/${name}";
        })
        hashedContents;

      plainPems = pkgs.runCommand "cacert-plain-pem"
        { nativeBuildInputs = [ pkgs.openssl ]; } ''
        mkdir -p $out
        for f in ${pkgs.cacert.unbundled}/etc/ssl/certs/*.crt; do
          name=$(basename "$f" .crt)
          openssl x509 -in "$f" -out "$out/$name.pem"
        done
      '';
      plainPemEntries = lib.mapAttrs'
        (name: _: lib.nameValuePair "ssl/certs/${name}" {
          source = "${plainPems}/${name}";
        })
        (builtins.readDir plainPems);
    in
    hashedEntries // plainPemEntries;

  hardware.sensor.iio.enable = true;

  services = {
    kmscon = {
      # Enable when sway issue fixed
      enable = false;
      hwRender = false;
    };

    hardware = {
      openrgb = {
        enable = true;
        package = pkgs.openrgb-with-all-plugins;
      };
    };

    acpid.enable = true;
    asusd.enable = true;

    fstrim.enable = true;
    devmon.enable = true;
    gvfs.enable = true;
    flatpak.enable = true;

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
      xdgOpenUsePortal = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      config.common.default = [ "wlr" "gtk" ];
    };
  };

  system.stateVersion = "23.11";
}
