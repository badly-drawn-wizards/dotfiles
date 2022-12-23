{ config, lib, pkgs, ... }:

{
  nix.settings.extra-sandbox-paths = [ (toString config.programs.ccache.cacheDir) ];
  programs.ccache = {
    enable = true;
    cacheDir = "/nix/var/cache/ccache";
    packageNames = [
      "linux_ccache"
    ];
  };
}
