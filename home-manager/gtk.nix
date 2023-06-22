{ config, lib, pkgs, ... }:

let
  studentNo = "stnreu001";
  uctWebdavUUIDs = {
    "dav:3dm" = "2ba50787-5140-4f63-b5bc-8b053bdc3b9d";
    "dav:3ms" = "8786c1c4-cfeb-4421-8893-44427e6a2ee4";
    "dav:csc" = "5002dbe3-736f-451d-a464-8b3d882f794c";
  };
  uctWebdavBookmarks = lib.mapAttrsToList (name: uuid: "davs://${studentNo}@vula.uct.ac.za/dav/${uuid} ${name}") uctWebdavUUIDs;
in
{
  config = {
    home.sessionVariables = {
      GTK_DPI_SCALE = 1;
      GTK_SCALE = 2;
    };
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
        iconTheme = draculaTheme;
        gtk2.extraConfig = toGtk2 extraConfig;
        gtk3 = {
          inherit extraConfig;
          bookmarks = [
            "file://${config.home.homeDirectory}/university"
            "file:///workspace"
            "file:///dot"
          ] ++ uctWebdavBookmarks;
        };
        gtk4 = { inherit extraConfig; };
      };
  };
}
