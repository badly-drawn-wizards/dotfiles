self: super:
{
  sway-unwrapped =
      super.sway-unwrapped.overrideAttrs (attrs: {
        src = super.fetchFromGitHub {
          owner = "badly-drawn-wizards";
          repo = "sway";
          rev = "47e4201076b2329386232b0fdb5ded5a7d08ca0e";
          sha256 = "0cjYy0pVeUeFvRdPw0U9IfyW/FBy2LmmYPfBl0KUbwU=";
        };
        mesonFlags = attrs.mesonFlags ++ [ "-Dtray=enabled" "-Dwerror=false" ];
      });

  swaylock =
    super.swaylock.overrideAttrs (attrs: {
      mesonFlags = [ "-Dpam=enabled" "-Dgdk-pixbuf=enabled" "-Dman-pages=enabled" ];
    });
}
