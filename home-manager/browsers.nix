{ pkgs
, config
, lib
, ...
}@args:
{

  programs.chromium.enable = true;

  programs.firefox = {
    enable = true;

    profiles."default" = {
      path = "lly2q038.default";
      userChrome = ''
      @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul"); /* set default namespace to XUL */
      #TabsToolbar {visibility: collapse;}
      #sidebar-header {
        display: none;
      }
    '';
    };
  };

  home.sessionVariables = {
    MOZ_USE_XINPUT2 = "1";
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_DBUS_REMOTE = "1";
  };

  home.packages = with pkgs; [
    tridactyl-native
  ];
}
