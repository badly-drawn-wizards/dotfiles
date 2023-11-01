{ config, lib, pkgs, ... }:

{
  console = {
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
    font = "ter-v32b";
    colors = with config.theme; [
      color0
      color1
      color2
      color3
      color4
      color5
      color6
      color7
      color8
      color9
      color10
      color11
      color12
      color13
      color14
      color15
    ];
    earlySetup = true;
  };

  fonts = {
    packages = with pkgs; [
      nerdfonts
      noto-fonts
      noto-fonts-emoji
      font-awesome
      fira-code
      fira-code-symbols
      emacs-all-the-icons-fonts
    ];
    enableDefaultPackages = true;
    fontconfig = {
      defaultFonts = {
        monospace = [ config.fontMono ];
      };
    };
  };

}
