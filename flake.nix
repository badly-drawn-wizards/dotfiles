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

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.noctalia-qs.follows = "noctalia-qs";
    };
    noctalia-qs = {
      url = "github:noctalia-dev/noctalia-qs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-azurevpn-client = {
      url = "github:cpuguy83/nix-azurevpn-client";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    { utils
    , nix-index
    , nixfs
    , vs-code-default-keybindings
    , nix-colors
    , nixpkgs-wayland
    , noctalia
    , noctalia-qs
    , nix-azurevpn-client
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
          noctalia-qs.overlays.default
          noctalia.overlays.default
          (self: super: {
            quickshell = super.quickshell.overrideAttrs (old: {
              patches = (old.patches or [ ]) ++ [ ./patches/qs-643.patch ];
            });
            noctalia-shell = super.noctalia-shell.override {
              quickshell = self.quickshell;
            };
          })
          nix-azurevpn-client.overlays.default
        ] ++ import ./overlays;

        hosts.noobnoob = {
          modules = [
            flake-plus-module
            nixfs.nixosModules.nixfs
            nix-azurevpn-client.nixosModules.azurevpnclient
            ./configuration.nix
          ];
          specialArgs = { inherit inputs; };
        };

        outputsBuilder = channels: {
          packages = channels.nixpkgs;
        };
      } // { inherit os; };
}
