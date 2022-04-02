self: super:
{
  sway-unwrapped =
      super.sway-unwrapped.overrideAttrs (attrs: {
        src = super.fetchFromGitHub {
          owner = "badly-drawn-wizards";
          repo = "sway";
          rev = "80f8c2f13f76318e90afb8e9c5c028bada2559a7";
          sha256 = "wx4MXc+934ooiOPyeJmf+kt2KZSx4bwezhBSXM/sbxc=";
        };
        mesonFlags = attrs.mesonFlags ++ [ "-Dtray=enabled" "-Dwerror=false" ];
      });

  swaylock =
    super.swaylock.overrideAttrs (attrs: {
      mesonFlags = [ "-Dpam=enabled" "-Dgdk-pixbuf=enabled" "-Dman-pages=enabled" ];
    });
}
