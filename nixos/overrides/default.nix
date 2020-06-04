{ config, lib, pkgs, ... }:
let
  # TODO Find out if there is something like this already in nixpkgs
  addEnvVars = name: pkg: envVars:
    let
      inherit (pkgs) lib;
      args = lib.concatStringsSep " " (lib.mapAttrsToList
        (k: v: "--set ${k} ${v}") envVars);
    in
    pkgs.symlinkJoin {
      name = name;
      paths = [ pkg ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/${name} ${args}
      '';
    };

  envVarOverlay = _: super: {
    firefox = addEnvVars "firefox" super.firefox {
      # For touchscreen support
      MOZ_USE_XINPUT2 = "1";
    };
    calibre = addEnvVars "calibre" pkgs.calibre {
      CALIBRE_USE_DARK_PALETTE = "1";
    };
  };


in
{
  nixpkgs.config.overlays = [
    envVarOverlay
    import ./doom-emacs.nix
  ];
}
