{ config, lib, pkgs, ... }:

let
  studentNo = "stnreu001";
  uctWebdavUUIDs = {
    "dav:3al" = "fd33d5ce-4d72-4f4f-8dfd-06ac420f9bed";
    "dav:3ms" = "980c4490-fe46-482d-a5f1-7233f044ba6c";
    "dav:csc" = "5002dbe3-736f-451d-a464-8b3d882f794c";
    # "dav:3dm" = "d9db9ea1-08a6-4aa1-8041-84a56d34006f";
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
        iconTheme = whitesurTheme;
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
