{ config, lib, pkgs, ... }:

{
  options = {
    onscreen-keyboard = {
      togglePackage = lib.mkOption {
        type = lib.types.package;
        readOnly = true;
        default = pkgs.writeScript "toggleKeyboard" ''
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
