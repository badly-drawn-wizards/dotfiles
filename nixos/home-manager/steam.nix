{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    # Gaaaaaaaaames (HMU if you play Celeste)
    steam
    steam-run
  ];

  home.file = {
    "./.local/bin/noita" = {
      text = ''
      #!/usr/bin/env sh
      cd $HOME/.local/share/Steam/steamapps/common/Noita/
      wine noita.exe
      '';
      executable = true;
    };
  };

}
