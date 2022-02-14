self: super:
{
  sway-unwrapped =
      super.sway-unwrapped.overrideAttrs (attrs: {
        src = super.fetchFromGitHub {
          owner = "badly-drawn-wizards";
          repo = "sway";
          rev = "f360b66f8d5fc3b3e73d46408549c603589270b8";
          sha256 = "uFJeWISK0yaN/oe1CNBp/WOLUamHWLrPqrg+SrV34d4=";
        };
        mesonFlags = attrs.mesonFlags ++ [ "-Dtray=enabled" "-Dwerror=false" ];
      });

  swaylock =
    super.swaylock.overrideAttrs (attrs: {
      mesonFlags = [ "-Dpam=enabled" "-Dgdk-pixbuf=enabled" "-Dman-pages=enabled" ];
    });
}
