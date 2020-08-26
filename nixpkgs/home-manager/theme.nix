{ config, lib, pkgs, ... }:

with lib;
{
  options.theme = mkOption {
    type = types.attrsOf types.string;
    default = {
      base00 = "292b2e";
      dark00 = "212026";
      base01 = "484848";
      base02 = "5d4d7a";
      dark02 = "444155";
      base03 = "5b5b5b";
      base04 = "a3a3a3";
      base05 = "b8b8b8";
      base06 = "e8e8e8";
      base07 = "f8f8f8";
      base08 = "f2241f";
      base09 = "ffa500";
      base0A = "b1951d";
      base0B = "67b11d";
      base0C = "2d9574";
      base0D = "4f97d7";
      base0E = "734d79";
      base0F = "b03060";
    };
  };
}
