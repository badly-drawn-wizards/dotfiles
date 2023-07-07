{ config, lib, pkgs, ... }:

let
  inherit (config.gtk) extraConfig;
in
{
  options = with lib; with types; {
    gtk.extraConfig = mkOption {
      type = attrsOf str;
      default = {};
    };
  };
  config = {
    home.sessionVariables = {
      GTK_DPI_SCALE = 1;
      GTK_SCALE = 2;
    };
    gtk =
      let
        toGtk2 = config: lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${k} = ${builtins.toString v}") config);
      in
      {
        enable = true;
        gtk2.extraConfig = toGtk2 extraConfig;
        gtk3 = {
          inherit extraConfig;
          bookmarks = [
            "file://${config.home.homeDirectory}/org"
            "file:///workspace"
            "file:///dot"
            "file://${config.home.homeDirectory}/Downloads"
            "file://${config.home.homeDirectory}/Dropbox"
            "sftp://reuben@gateway.whirlylabs.com:25218/whirly whirly"
            "sftp://reuben@gateway.whirlylabs.com:25218/reuben whirly-home"
          ];
        };
        gtk4 = { inherit extraConfig; };
      };
  };
}
