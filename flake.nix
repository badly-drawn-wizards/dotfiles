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
    nixpkgs-wayland  = {
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
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      #url = "github:badly-drawn-wizards/nix-doom-emacs";
      #url = "/workspace/nix-doom-emacs";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
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
    pypi-deps-db = {
      url = "github:DavHau/pypi-deps-db";
      flake = false;
    };
    mach-nix = {
      url = "github:DavHau/mach-nix";
      inputs.nixpkgs.follows = "/nixpkgs";
      inputs.pypi-deps-db.follows = "/pypi-deps-db";
    };
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
    linux = {
      url = "git+file:///workspace/linux?ref=master";
      flake = false;
    };
    lean4 = {
      url = "github:leanprover/lean4";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    vs-code-default-keybindings = {
      url = "github:codebling/vs-code-default-keybindings";
      flake = false;
    };

    nix-colors.url = "github:Misterio77/nix-colors";
  };
  outputs = {
    nixpkgs,
      utils,
      emacs-overlay,
      nix-doom-emacs,
      nur,
      nixpkgs-wayland,
      nix-index,
      nixfs,
      mach-nix,
      lean4,
      vs-code-default-keybindings,
      nix-colors,
      unhinged,
      linux,
      self,
      ...
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
        config = nixos.config;
        lpkgs = config.boot.kernelPackages;
        hm = config.home-manager.users.reuben;
        emacs = hm.programs.doom-emacs.package;
        epkgs = pkgs.emacsPackages;
      };
    in
      utils.lib.mkFlake {
        inherit self inputs;

        channelsConfig.allowUnfree = true;
        channels.nixpkgs = {
          patches = import ./patches;
        };

        sharedOverlays = [
          (self: super: {
            inherit os;
            inherit (nix-index) nix-index nix-locate;
            inherit vs-code-default-keybindings;
            inherit nix-colors;
            linuxSrc_custom = linux;
            lean4 = super.callPackage ({system}: lean4.packages.${system}) {};
            mach-nix = super.callPackage ({system}: mach-nix.lib.${system});
          })
          (emacs-overlay.overlay)
          (nur.overlay)
          (nixpkgs-wayland.overlay)
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
          devShell = channels.nixpkgs.mkShell {
            buildInputs = [
              channels.nixpkgs.nur.repos.rycee.mozilla-addons-to-nix
            ];
          };
        };
      } // { inherit os; };
}
