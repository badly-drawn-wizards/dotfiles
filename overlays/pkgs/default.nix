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
  otf2bdf = callPackage (import ./otf2bdf) {};
  fira-consolefont = callPackage (import ./fira-consolefont.nix) {};

  # If there is a proper way to override, I could not find it.
  vscode-extensions = super.vscode-extensions // {
    ms-dotnettools = super.vscode-extensions.ms-dotnettools // {
      csharp = callPackage (import ./ms-dotnettools-csharp) {};
    };
  };
}
