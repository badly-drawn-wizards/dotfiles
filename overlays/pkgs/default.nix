self: super:

let
  inherit (self) callPackage;
in
{
  dracula-grub-theme = callPackage (import ./dracula-grub-theme) { };
  dracula-wizard-plymouth = callPackage (import ./dracula-wizard-plymouth) { };
  dracula-rofi-theme = callPackage (import ./dracula-rofi-theme.nix) { };
  dracula-theme = callPackage (import ./dracula-theme.nix) { };
  screenshot = callPackage (import ./screenshot.nix) { };
  logitune = callPackage (import ./logitune.nix) { };
}
