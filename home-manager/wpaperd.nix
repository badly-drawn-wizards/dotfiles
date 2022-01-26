{ config, lib, pkgs, ... }:
{

  window-manager.startupPrograms = [
    "${pkgs.wpaperd}/bin/wpaperd"
  ];

  home.file = {
    ".config/wpaperd/output.conf".text = ''
      [default]
      path = ${config.home.homeDirectory}/background
    '';
  };
}
