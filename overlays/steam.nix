self: super: {
  steam = super.steam.override {
    extraPkgs = pkgs: with pkgs; [
      gamescope
      mangohud
    ];
  };
}
