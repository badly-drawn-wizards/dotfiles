{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    nixpkgs-master = {
      url = "github:NixOS/nixpkgs/master";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
      };
    };
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
    };
    nur = {
      url = "github:nix-community/NUR";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
      };
    };
    doom-emacs = {
      url = "github:doomemacs/doomemacs/develop";
      flake = false;
    };
    nix-straight = {
      url = "github:badly-drawn-wizards/nix-straight.el";
      flake = false;
    };
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      #url = "github:badly-drawn-wizards/nix-doom-emacs";
      #url = "/workspace/nix-doom-emacs";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
        nix-straight.follows = "/nix-straight";
        # doom-emacs.follows = "/doom-emacs";
        # emacs-overlay.follows = "/emacs-overlay";
      };
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
    hyprland.url = "github:hyprwm/Hyprland";
    unhinged.url = "github:badly-drawn-wizards/unhinged";

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
      url = "github:codebling/vs-code-default-keybindings";
      flake = false;
    };

    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-colors.url = "github:Misterio77/nix-colors";

    dotfiles-private = {
      url = "git+file:///workspace/dotfiles-private?ref=master";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixd = {
      url = "github:nix-community/nixd";
    };

    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    k8s-vm = {
      url = "git+file:///workspace/k8s-vm";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.microvm.follows = "microvm";
    };

  };

  outputs =
    { nixpkgs
    , nixpkgs-master
    , utils
    , emacs-overlay
    , nur
    , nixpkgs-wayland
    , nix-index
    , nixfs
    , lean4
    , vs-code-default-keybindings
    , nix-colors
    , unhinged
    , nixd
    , # linux,
      self
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
              path = "/workspace/dotfiles";
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

        channelsConfig.allowUnfree = true;
        channels.nixpkgs = {
          patches = import ./patches;
        };

        sharedOverlays = [
          # (emacs-overlay.overlay)
          (nur.overlay)
          (nixpkgs-wayland.overlay)
          (self: super: {
            inherit os;
            inherit (nix-index) nix-index nix-locate;
            inherit vs-code-default-keybindings;
            inherit nix-colors;
            pkgs-master = import nixpkgs-master { inherit (self) system; };
            # linuxSrc_custom = linux;
            lean4 = super.callPackage ({ system }: lean4.packages.${system}) { };
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
          # devShell = channels.nixpkgs.mkShell {
          #   buildInputs = [
          #     channels.nixpkgs.nur.repos.rycee.mozilla-addons-to-nix
          #   ];
          # };
        };
      } // { inherit os; };
}
