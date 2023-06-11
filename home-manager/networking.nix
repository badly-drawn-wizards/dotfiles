{ config, lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      networkmanager_dmenu
    ];
    file = {
      "./.local/bin/nmd" = {
        text = ''
          #!/usr/bin/env sh
          ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu
        '';
        executable = true;
      };
      "./.config/networkmanager-dmenu/config.ini".text = ''
        [dmenu]
        dmenu_command = ${config.programs.rofi.finalPackage}/bin/rofi -dmenu
      '';
    };
  };

}
