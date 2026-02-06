{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    # nixpkgs-staging-next = {
    #   url = "github:NixOS/nixpkgs/staging-next";
    # };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
      };
    };
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
      };
    };
    nix-straight = {
      url = "github:badly-drawn-wizards/nix-straight.el";
      flake = false;
    };
    nix-doom-emacs-unstraightened = {
      url = "github:marienz/nix-doom-emacs-unstraightened";
      inputs.nixpkgs.follows = "";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    dwarffs = {
      url = "github:edolstra/dwarffs";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    #hyprland.url = "github:hyprwm/Hyprland";

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

    #    dotfiles-private = {
    #      url = "git+file:///workspace/dotfiles-private?ref=master";
    #    };

    nixvim = {
      url = "github:nix-community/nixvim";
      # url = "git+file:///workspace/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixd = {
      url = "github:nix-community/nixd";
    };

    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    waveforms.url = "github:liff/waveforms-flake";

    # k8s-vm = {
    #   url = "git+file:///workspace/k8s-vm";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.microvm.follows = "microvm";
    # };

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
          # (emacs-overlay.overlay)
          # (nixpkgs-wayland.overlay)
          (self: super: {
            inherit os;
            inherit (nix-index) nix-index nix-locate;
            inherit vs-code-default-keybindings;
            inherit nix-colors;
            # staging-next = import nixpkgs-staging-next { inherit (self) system; };
            # linuxSrc_custom = linux;
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
