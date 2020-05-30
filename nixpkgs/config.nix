{
  allowUnfree = true;
  packageOverrides = pkgs: {
    calibre = pkgs.symlinkJoin {
      name = "calibre";
      paths = [ pkgs.calibre ];
      buildInputs = [ pkgs.makeWrapper ];
      # Set dark mode
      postBuild = ''
        wrapProgram $out/bin/calibre --set CALIBRE_USE_DARK_PALETTE 1
      '';
    };
    firefox = pkgs.symlinkJoin {
      name = "firefox";
      paths = [ pkgs.firefox ];
      buildInputs = [ pkgs.makeWrapper ];
      # Enable touchscreen support
      postBuild = ''
        wrapProgram $out/bin/firefox --set MOZ_USE_XINPUT2 1
      '';
    };
  };
}
