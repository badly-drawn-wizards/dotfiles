{ config, lib, pkgs, ... }:

{
  console = {
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
    font = "ter-i32b";
    earlySetup = true;
  };

  fonts = {
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
      font-awesome
      source-code-pro
      noto-fonts
      noto-fonts-emoji
    ];
    enableDefaultFonts = true;
    fontconfig = {
      defaultFonts = {
        monospace = [ "Fira Code" ];
      };
    };
  };

}
