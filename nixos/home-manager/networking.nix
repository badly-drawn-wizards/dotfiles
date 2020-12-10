{ config, lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      networkmanager_dmenu
    ];
    files = {
      "./.local/bin/nmd".source = ''
        #!/usr/bin/env sh
        ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu
      '';
      "./config/networkmanager-dmenu/config.ini".source = ''
        [dmenu]
        dmenu_command = rofi -dmenu
      '';
    };
  };

}
