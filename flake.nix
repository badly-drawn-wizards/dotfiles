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
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      # url = "/workspace/nix-doom-emacs";
      inputs = {
        nixpkgs.follows = "/nixpkgs";
        doom-emacs.follows = "/doom-emacs";
        emacs-overlay.follows = "/emacs-overlay";
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    nix-ld = {
      url = "github:Mic92/nix-ld";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
    dwarffs.url = "github:edolstra/dwarffs";
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
  };
  outputs = { nixpkgs, utils, emacs-overlay, nur, nixpkgs-wayland, mach-nix, self, ... }@inputs:
    utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;
      channels.nixpkgs = {
        patches = import ./patches;
      };

      sharedOverlays = [
        (self: super: { mach-nix = self.callPackage ({system}: mach-nix.lib.${system}) {}; })
        (emacs-overlay.overlay)
        (nur.overlay)
        (nixpkgs-wayland.overlay)
      ] ++ import ./overlays;

      hosts.noobnoob = {
        modules = [ ./configuration.nix ];
        specialArgs = { inherit inputs; };
      };

      outputsBuilder = channels: {
        packages = channels.nixpkgs;
      };
    } // {
      # :lf <path-to-dotfiles>
      # :a repl
      repl = rec {
        pkgs = self.pkgs.x86_64-linux.nixpkgs;
        config = self.nixosConfigurations.noobnoob.config;
        hm = config.home-manager.users.reuben;
        emacs = hm.programs.emacs.package;
        epkgs = pkgs.emacsPackagesFor emacs;
      };
    };
}
