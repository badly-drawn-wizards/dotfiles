self: super:

let
  inherit (self) callPackage;
in
{
  rot8 = callPackage (import ./rot8.nix) {};
  dracula-grub-theme = callPackage (import ./dracula-grub-theme) {};
  dracula-rofi-theme = callPackage (import ./dracula-rofi-theme.nix) {};
  lightnovel-crawler = callPackage (import ./lightnovel-crawler) {};

  nbrowse = callPackage (import ./nbrowse) {};

  # If there is a proper way to override, I could not find it.
  vscode-extensions = super.vscode-extensions // {
    ms-dotnettools = super.vscode-extensions.ms-dotnettools // {
      csharp = callPackage (import ./ms-dotnettools-csharp) {};
    };
  };
}
