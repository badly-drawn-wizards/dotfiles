self: super:
{
  sway-unwrapped =
      super.sway-unwrapped.overrideAttrs (attrs: {
        src = super.fetchFromGitHub {
          owner = "FlexW";
          repo = "sway";
          rev = "a5bb9654acfdb3bf9fb1714f0f6f1aa5fcec8835";
          sha256 = "aOlvyO50dMQKWSfX/H9ZSG8st7nP6zgAwX+gZjDap8I=";
        };
        mesonFlags = attrs.mesonFlags ++ [ "-Dtray=enabled" "-Dwerror=false" ];
      });

  swaylock =
    super.swaylock.overrideAttrs (attrs: {
      mesonFlags = [ "-Dpam=enabled" "-Dgdk-pixbuf=enabled" "-Dman-pages=enabled" ];
    });
}
