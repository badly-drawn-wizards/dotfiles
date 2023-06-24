{ config, lib, pkgs, ... }:

{
  windowManager.extraBinds = {
    "XF86PowerOff" = "exec rot8 -O";
  };

  home.packages = [ pkgs.rot8 ];
}
