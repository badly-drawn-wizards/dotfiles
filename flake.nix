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
    lean4 = {
      url = "github:leanprover/lean4";
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

    nixd = {
      url = "github:nix-community/nixd";
    };

  };

  outputs =
    { utils
    , nix-index
    , nixfs
    , lean4
    , vs-code-default-keybindings
    , nix-colors
    , nixd
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
        nixd = {
          nixos = nixos.options;
          hm = nixos.options.home-manager.users.type.getSubOptions [ ];
        };
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
            lean4-flake = super.callPackage ({ system }: lean4.packages.${system}) { };
          })
          nixd.overlays.default
        ] ++ import ./overlays;

        hosts.noobnoob = {
          modules = [
            flake-plus-module
            nixfs.nixosModules.nixfs
            ./configuration.nix
          ];
          specialArgs = { inherit inputs; };
        };

        outputsBuilder = channels: {
          packages = channels.nixpkgs;
        };
      } // { inherit os; };
}
