{ lib, pkgs }:

let
  inherit (builtins) floor;
  inherit (pkgs.nix-colors.lib.conversions) hexToRGB;
  inherit (lib)
    length min max toHexString;
  inherit (lib.strings)
    removePrefix concatMapStrings fixedWidthString;
  inherit (lib.lists)
    genList zipListsWith reverseList elemAt;
in
{
  # Convert RGB list [r g b] to hex string "rrggbb"
  rgb-to-hex = concatMapStrings (v: fixedWidthString 2 "0" (toHexString v));

  # Mix two RGB color lists with alpha blending
  # a: alpha value (0.0 = color1, 1.0 = color2)
  # Returns: mixed RGB list
  color-mix-rgb = a: zipListsWith (c1: c2: floor ((1 - a) * c1 + a * c2));

  # Mix two hex colors with alpha blending
  # a: alpha value (0.0 = h1, 1.0 = h2)
  # h1, h2: hex colors (with or without # prefix)
  # Returns: hex color string with # prefix
  color-mix-hex = a: h1: h2:
    let
      rgb-to-hex = concatMapStrings (v: fixedWidthString 2 "0" (toHexString v));
      color-mix-rgb = a: zipListsWith (c1: c2: floor ((1 - a) * c1 + a * c2));
    in
    "#" + rgb-to-hex (color-mix-rgb a (hexToRGB (removePrefix "#" h1)) (hexToRGB (removePrefix "#" h2)));

  # Clamp value between min and max
  clamp = x: y: a: max x (min y a);

  # Linear interpolation through a list of values
  # f: interpolation function (takes alpha, value1, value2)
  # xs: list of values to interpolate between
  # a: interpolation value (0.0 to 1.0)
  # Returns: interpolated value
  lerp-list-with = f: xs: a:
    let
      clamp = x: y: a: max x (min y a);
      a' = clamp 0 1 a;
      n = length xs - 1;
      x1 = floor (a' * n);
      x2 = min n (x1 + 1);
      b = a' * n - x1;
    in
    assert n > 0;
    f b (elemAt xs x1) (elemAt xs x2);

  # Generate n evenly spaced values from 0 to 1
  arange1 = n: genList (x: 1.0 * x / (n - 1)) n;

  # Generate color gradient tiers from red -> yellow -> white
  # theme: attrset with red, yellow, white colors
  # n: number of tiers (default 15)
  # Returns: list of hex colors
  color-tiers-asc = theme: n:
    let
      color-mix-hex = a: h1: h2:
        let
          rgb-to-hex = concatMapStrings (v: fixedWidthString 2 "0" (toHexString v));
          color-mix-rgb = a: zipListsWith (c1: c2: floor ((1 - a) * c1 + a * c2));
        in
        "#" + rgb-to-hex (color-mix-rgb a (hexToRGB (removePrefix "#" h1)) (hexToRGB (removePrefix "#" h2)));
      lerp-list-with = f: xs: a:
        let
          clamp = x: y: a: max x (min y a);
          a' = clamp 0 1 a;
          n = length xs - 1;
          x1 = floor (a' * n);
          x2 = min n (x1 + 1);
          b = a' * n - x1;
        in
        assert n > 0;
        f b (elemAt xs x1) (elemAt xs x2);
      arange1 = n: genList (x: 1.0 * x / (n - 1)) n;
    in
    builtins.map
      (lerp-list-with color-mix-hex [ theme.red theme.yellow theme.white ])
      (arange1 n);

  # Generate color gradient tiers from white -> yellow -> red
  # theme: attrset with red, yellow, white colors
  # n: number of tiers (default 15)
  # Returns: list of hex colors
  color-tiers-desc = theme: n: reverseList (
    let
      color-mix-hex = a: h1: h2:
        let
          rgb-to-hex = concatMapStrings (v: fixedWidthString 2 "0" (toHexString v));
          color-mix-rgb = a: zipListsWith (c1: c2: floor ((1 - a) * c1 + a * c2));
        in
        "#" + rgb-to-hex (color-mix-rgb a (hexToRGB (removePrefix "#" h1)) (hexToRGB (removePrefix "#" h2)));
      lerp-list-with = f: xs: a:
        let
          clamp = x: y: a: max x (min y a);
          a' = clamp 0 1 a;
          n = length xs - 1;
          x1 = floor (a' * n);
          x2 = min n (x1 + 1);
          b = a' * n - x1;
        in
        assert n > 0;
        f b (elemAt xs x1) (elemAt xs x2);
      arange1 = n: genList (x: 1.0 * x / (n - 1)) n;
    in
    builtins.map
      (lerp-list-with color-mix-hex [ theme.red theme.yellow theme.white ])
      (arange1 n)
  );
}
