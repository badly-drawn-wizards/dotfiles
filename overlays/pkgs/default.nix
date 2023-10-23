self: super:

let
  inherit (self) callPackage;
in
{
  rot8 = callPackage (import ./rot8.nix) {};
  dracula-grub-theme = callPackage (import ./dracula-grub-theme) {};
  dracula-rofi-theme = callPackage (import ./dracula-rofi-theme.nix) {};
  screenshot = callPackage (import ./screenshot.nix) {};
}
