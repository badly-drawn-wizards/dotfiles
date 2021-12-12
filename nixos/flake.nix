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
      # url = "github:vlaci/nix-doom-emacs/develop";
      url = "/workspace/nix-doom-emacs";
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
    dwarffs.url = "github:edolstra/dwarffs";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
  };
  outputs = { nixpkgs, utils, nur, emacs-overlay, self, ... }@inputs:
    utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;
      channels.nixpkgs = {
        patches = import ./patches;
      };

      sharedOverlays = [
        (emacs-overlay.overlay)
        (nur.overlay)
      ] ++ import ./overlays;

      hosts.noobnoob = {
        modules = [ ./configuration.nix ];
        specialArgs = { inherit inputs; };
      };
    };
}
