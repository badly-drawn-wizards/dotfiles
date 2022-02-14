{ config, lib, pkgs, ... }:

with lib;
{
  config = {
    gtk =
      let
        draculaTheme = {
          package = pkgs.dracula-theme;
          name = "Dracula";
        };
        whitesurTheme = {
          package = pkgs.whitesur-icon-theme;
          name = "WhiteSur-dark";
        };
        extraConfig = {
        };
        toGtk2 = config: lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${k} = ${builtins.toString v}") config);
      in
      {
        enable = true;
        theme = draculaTheme;
        iconTheme = whitesurTheme;
        gtk2.extraConfig = toGtk2 extraConfig;
        gtk3 = {
          inherit extraConfig;
          bookmarks = [
            "file://${config.home.homeDirectory}/university"
            "file:///workspace"
            "file:///dot"
          ];
        };
        gtk4 = { inherit extraConfig; };
      };
  };
}
