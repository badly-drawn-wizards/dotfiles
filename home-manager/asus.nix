{ pkgs, ... }:

{
  windowManager.startupPrograms = [
    "${pkgs.asusctl}/bin/rog-control-center"
  ];

  home.packages = [ pkgs.asusctl ];
}
