{ pkgs, config, ... }:

{
  programs.noctalia = {
    enable = true;
  };

  windowManager.startupPrograms = [
    "${config.programs.noctalia.package}/bin/noctalia"
  ];
}
