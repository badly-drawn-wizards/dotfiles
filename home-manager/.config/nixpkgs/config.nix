{
  allowUnfree = true;
  packageOverrides = pkgs: {
    calibre = pkgs.symlinkJoin {
      name = "calibre";
      paths = [ pkgs.calibre ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/calibre --set CALIBRE_USE_DARK_PALETTE 1
      '';
    };
  };
}
