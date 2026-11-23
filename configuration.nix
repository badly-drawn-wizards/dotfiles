{ pkgs
, lib
, inputs
, ...
}:


{
  # Latest OpenCode (2.4.0 from nixpkgs master) instead of the nixpkgs pin's 2.2.5.
  # `pkgs/opencode.nix` is master's derivation with its verified src/node_modules
  # hashes, so this is a version bump rather than a rebuild of the packaging.
  nixpkgs.overlays = [
    (final: prev: { opencode = prev.callPackage ./pkgs/opencode.nix { }; })
  ];

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
      ./millionants-builder.nix
      ./users.nix
      ./security.nix
      ./nixos-memory.nix
      ./slurm.nix
      # ./speech.nix # TODO: re-enable once piper-tts has a cached torch (it compiles torch from source)
    ];

  # The WAN path MTU is below 1500: `ping -M do -s 1472` to 8.8.8.8 fails, 1400 passes.
  # A full-size TLS handshake is dropped, so every HTTPS call (GitHub, Cachix, `nix copy`)
  # times out while ICMP and the LAN look healthy. NetworkManager resets the link MTU on
  # every reconnect, so a dispatcher script re-applies it; the udev rule covers the first
  # appearance before a connection profile exists.
  networking.networkmanager.dispatcherScripts = [
    {
      type = "basic";
      source = pkgs.writeShellScript "mtu-1400" ''
        [ "$1" = "eth0" ] && ${pkgs.iproute2}/bin/ip link set dev eth0 mtu 1400 || true
      '';
    }
  ];
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="net", KERNEL=="eth0", ATTR{mtu}="1400"
  '';

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

    # nixos-rebuild wrapper with nix-output-monitor progress and a package
    # diff. Backs the nrb/nrs aliases. GC stays with nix.gc, not nh clean.
    nh = {
      enable = true;
      flake = "/etc/nixos";
    };

    evolution = {
      enable = true;
      plugins = [ pkgs.evolution-ews ];
    };

    # azurevpnclient.enable = true;
    # azurevpnclient.package = pkgs.microsoft-azurevpnclient;
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

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;

  services = {
    kmscon = {
      # Enable when sway issue fixed
      enable = false;
      config.hwaccel = false;
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

      packages = with pkgs; [ dconf gcr_4 ];
    };

    # fprintd.enable = true;

    fwupd.enable = true;

    wyoming.piper.servers.piper = {
      # TODO: re-enable with speech.nix; piper-tts needs torch built from source.
      enable = false;
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

  # SSH for the tailnet only: the Steam Deck mounts the worktrees over SFTP/sshfs. The
  # firewall is enabled, so port 22 is opened on tailscale0 and nowhere else; key-only
  # auth, no root login, SFTP on for sshfs.
  services.openssh = {
    enable = true;
    allowSFTP = true;
    # `openFirewall` defaults to true and would open 22 on every interface; the
    # tailscale0 rule below is the only opening.
    openFirewall = false;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };
  # 6817/6818 are Slurm's controller and node ports and are opened on
  # tailscale0 by modules/slurm.nix (the nodes share that rule). They must be on
  # the tailnet interface, not just loopback: a connection to this host's own
  # tailnet address does not traverse `lo`, and the worker nodes reach the
  # controller only over the tailnet (2026-11-18: with only 22 allowed, `scontrol`
  # timed out and both Windows nodes could not register).
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ 22 ];

  system.stateVersion = "23.11";
}
