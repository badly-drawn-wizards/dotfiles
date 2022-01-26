{ config, lib, pkgs, ... }:

{
  window-manager.extraBinds = {
    "XF86PowerOff" = "exec toggle-rot8";
  };

  systemd.user.services.rot8 = {
    Unit = {
      Description = "rot8, automatic screen rotation";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = { WantedBy = [ "graphical-session.target" ]; };

    Service = {
      ExecStart = "${pkgs.rot8}/bin/rot8";
    };
  };

  home.file = {
    ".local/bin/toggle-rot8" = {
      text = ''
        #!/usr/bin/env /bin/sh
        if (systemctl --user -q is-active rot8)
        then
          systemctl --user stop rot8
        else
          systemctl --user start rot8
        fi
      '';
      executable = true;
    };
  };
}
