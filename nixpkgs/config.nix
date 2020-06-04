{
  allowUnfree = true;
  permittedInsecurePackages = [
    # For OpenBoard
    "xpdf-4.02"
  ];
  packageOverrides = {pkgs, lib, ...}:
    let
      sources = import ./nix/sources.nix;

      addEnvVars = name: pkg: envVars:
        let
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

      firefox = addEnvVars "firefox" pkgs.firefox {
        # For touchscreen support
        MOZ_USE_XINPUT2 = "1";
      };

      calibre = addEnvVars "calibre" pkgs.calibre {
        CALIBRE_USE_DARK_PALETTE = "1";
      };
      emacs = pkgs.callPackage
        (builtins.fetchTarball
          {
            url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
          })
        {
          doomPrivateDir = ./doom;
          extraPackages = epkgs: [ epkgs.doom-themes ];
        };
    in
    {
      inherit firefox calibre emacs;
      pkgs-pinned = sources.nixpkgs;
      pkgs-forked = sources.pkgs-forked;
      home-manager = sources.home-manager;
    };
}
