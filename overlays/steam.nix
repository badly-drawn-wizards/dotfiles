self: super: {
  steam = super.steam.override {
    extraPkgs = pkgs: with pkgs; [
      gamescope
      mangohud
    ];
    extraEnv = {
      STEAM_EXTRA_COMPAT_TOOLS_PATHS = self.lib.makeSearchPathOutput "steamcompattool" "" [self.proton-ge-bin];
    };
  };
}
