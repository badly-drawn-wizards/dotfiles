{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    nix-index.url = "github:bennofs/nix-index";
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixfs = {
      url = "github:illustris/nixfs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    comma = {
      url = "github:nix-community/comma";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    vs-code-default-keybindings = {
      url = "github:badly-drawn-wizards/vs-code-default-keybindings";
      flake = false;
    };

    nix-colors.url = "github:Misterio77/nix-colors";

    nixvim = {
      url = "github:nix-community/nixvim";
      # url = "git+file:///workspace/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Wiz: tower (toolchain manager) and the wiz.nvim plugin. Its own nixpkgs pin is kept
    # (no follows), so builds match the badlydrawnwizards Cachix cache.
    wiz.url = "github:wiz-pl/wiz";

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-azurevpn-client = {
      url = "github:cpuguy83/nix-azurevpn-client";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Secure Boot: signed systemd-boot stubs, keys from sbctl.
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # The NixOS-WSL integration for the win2/win3 Slurm nodes. Pinned to the
    # revision those nodes have been built with; `follows` keeps one nixpkgs so
    # slurmd/munge and the store paths a job names match the controller.
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/05721a428830972f025a8575b1a19c418d422d1b";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Private config kept in a separate repo. Local path rather than a remote so the
    # working tree is picked up directly; no lock churn when it changes.
    dotfiles-private.url = "path:/workspace/dotfiles-private";

  };

  outputs =
    { utils
    , nix-index
    , nixfs
    , vs-code-default-keybindings
    , nix-colors
    , nixpkgs
    , nixpkgs-wayland
    , noctalia
    , nix-azurevpn-client
    , lanzaboote
    , nixos-wsl
    , dotfiles-private
    , self
    , ...
    }@inputs:
    let
      flake-plus-module =
        (_: {
          nix = {
            generateRegistryFromInputs = true;
            linkInputs = true;
            registry.dot.to = {
              type = "path";
              path = "/etc/nixos";
            };
          };
        });
      os = rec {
        inherit self;
        nixos = self.nixosConfigurations.noobnoob;
        pkgs = nixos.pkgs;
        lib = pkgs.lib;
        config = nixos.config;
        lpkgs = config.boot.kernelPackages;
        hm = config.home-manager.users.reuben;
        emacs = hm.programs.doom-emacs.package;
        epkgs = pkgs.emacsPackages;
      };

      # The Slurm nodes. Their configuration is the same files the thin
      # wsl-flake entry point imports (hosts/win2.nix, hosts/win3.nix,
      # modules/slurm/node-common.nix), built from the plain nixpkgs input:
      # the node closure must not pick up the controller's desktop overlays.
      mkSlurmNode = name: nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nixos-wsl.nixosModules.default
          (./hosts + "/${name}.nix")
        ];
      };

      deck = mkSlurmNode "deck";

      base = utils.lib.mkFlake
        {
          inherit self inputs;

          channelsConfig = {
            allowUnfree = true;
            android_sdk.accept_license = true;
          };
          channels.nixpkgs = {
            patches = import ./patches;
          };

          sharedOverlays = [
            (self: super: {
              inherit os;
              inherit (nix-index) nix-index nix-locate;
              inherit vs-code-default-keybindings;
              inherit nix-colors;
            })
            nixpkgs-wayland.overlay
            nix-azurevpn-client.overlays.default
          ] ++ import ./overlays;

          hosts.noobnoob = {
            modules = [
              flake-plus-module
              nixfs.nixosModules.nixfs
              nix-azurevpn-client.nixosModules.azurevpnclient
              lanzaboote.nixosModules.lanzaboote
              dotfiles-private.nixosModules.default
              ./hosts/noobnoob.nix
            ];
            specialArgs = { inherit inputs; };
          };

          outputsBuilder = channels: {
            # `nix build .#opencode` uses the same derivation as the NixOS
            # overlay in configuration.nix, not the older nixpkgs package.
            packages = channels.nixpkgs // {
              opencode = channels.nixpkgs.callPackage ./pkgs/opencode.nix { };
            };
          };
        };
    in
    base
    // {
      inherit os;

      # win2/win3 are in the main flake so the controller (and CI) can evaluate
      # and build the same node systems a node switches to. The wsl-flake
      # wrapper is the on-node entry point; it imports these host files rather
      # than redefining them.
      nixosConfigurations = base.nixosConfigurations // {
        win2 = mkSlurmNode "win2";
        win3 = mkSlurmNode "win3";
      };

      # Not a nixosConfiguration on purpose: the Deck runs SteamOS and has no
      # NixOS to switch. See hosts/deck.nix.
      inertConfigurations.deck = deck;

      checks.x86_64-linux = {
        # Force the inert deck description to evaluate without requiring a
        # bootloader/filesystem layout for real SteamOS hardware.
        deck-is-evaluable =
          nixpkgs.legacyPackages.x86_64-linux.writeText "deck-node-entry"
            deck.config.slurmCluster.nodeEntry;
      };
    };
}
