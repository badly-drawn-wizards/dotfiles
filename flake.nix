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
    , nixpkgs-wayland
    , noctalia
    , nix-azurevpn-client
    , lanzaboote
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
    in
    utils.lib.mkFlake
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
            ./configuration.nix
          ];
          specialArgs = { inherit inputs; };
        };

        outputsBuilder = channels: {
          packages = channels.nixpkgs;
        };
      } // { inherit os; };
}
