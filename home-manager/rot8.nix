{ config, lib, pkgs, ... }:

{
  windowManager.extraBinds = {
    "XF86PowerOff" = "exec toggle-rot8";
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

  home.packages = [ pkgs.rot8 ];
}
