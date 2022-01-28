self: super:

let
  inherit (self) callPackage;
in
{
  wpaperd = callPackage (import ./wpaperd.nix) {};
  rot8 = callPackage (import ./rot8.nix) {};
  wizard = callPackage (import ./wizard.nix) {};
  dracula-grub-theme = callPackage (import ./dracula-grub-theme.nix) {};
  dracula-rofi-theme = callPackage (import ./dracula-rofi-theme.nix) {};
  lightnovel-crawler = callPackage (import ./lightnovel-crawler.nix) {};
}
