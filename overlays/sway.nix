self: super:
{
  # This isn't visible for some reason. I somehow broke my overlays or something.
  sway-unwrapped =
    super.sway-unwrapped.overrideAttrs (_: {
      src = super.fetchFromGitHub {
        owner = "FlexW";
        repo = "sway";
        rev = "a5bb9654acfdb3bf9fb1714f0f6f1aa5fcec8835";
        sha256 = "aOlvyO50dMQKWSfX/H9ZSG8st7nP6zgAwX+gZjDap8I=";
      };
    });
}
