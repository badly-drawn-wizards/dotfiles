{ config, lib, pkgs, ... }:

{
  options = {
    onscreen-keyboard = {
      togglePackage = lib.mkOption {
        type = lib.types.package;
        default = pkgs.writeScriptBin "toggleKeyboard" ''
          #!/usr/bin/env bash
          function buscall() {
             local cmd=$1
             shift
             ${pkgs.systemd}/bin/busctl $cmd "--user" "sm.puri.OSK0" "/sm/puri/OSK0" "sm.puri.OSK0" $@
          }
          vis=$(buscall get-property Visible --json=short | ${pkgs.jq}/bin/jq '.data | not')
          buscall call SetVisible b $vis
        '';
      };
    };
  };
  config = {
    windowManager.startupPrograms = [
    #  { command = "${pkgs.fcitx5}/bin/fcitx"; always = true; }
    ];

    systemd.user.services.squeekboard = {
      Unit = {
        Description = "squeekboard";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart = "${pkgs.squeekboard}/bin/squeekboard";
        Restart = "on-failure";
      };
    };

    home.packages = [ config.onscreen-keyboard.togglePackage ];

    home.file = {
      # ".local/share/squeekboard/keyboards/sway.yaml".source = ./sway.yaml;
    };

    dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/input-sources" = {
          sources =
            map lib.hm.gvariant.mkTuple [
              ["xkb" "us_wide"]
              ["xkb" "terminal/us_wide"]
            ];
        };
      };
    };

  };
}
