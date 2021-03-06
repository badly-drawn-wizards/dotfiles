{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    # Gaaaaaaaaames (HMU if you play Celeste)
    steam
    steam-run-native

    # For proton
    python3
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
