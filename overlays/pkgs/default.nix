self: super:

let
  inherit (self) callPackage;
in
{
  dracula-grub-theme = callPackage (import ./dracula-grub-theme) { };
  dracula-rofi-theme = callPackage (import ./dracula-rofi-theme.nix) { };
  screenshot = callPackage (import ./screenshot.nix) { };

  kata-runtime = callPackage (import ./kata-runtime) { };
  kata-images = callPackage (import ./kata-images) { };

  webnovel-android = callPackage (import ./webnovel-android.nix) { };
  pomo = callPackage (import ./pomo) { };
}
