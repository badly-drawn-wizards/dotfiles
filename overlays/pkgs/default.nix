self: super:

let
  inherit (self) callPackage;
in
{
  dracula-grub-theme = callPackage (import ./dracula-grub-theme) { };
  dracula-rofi-theme = callPackage (import ./dracula-rofi-theme.nix) { };
  screenshot = callPackage (import ./screenshot.nix) { };

  pomo = callPackage (import ./pomo) { };
}
