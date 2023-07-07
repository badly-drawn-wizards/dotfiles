{ pkgs
, config
, lib
, ...
}@args:
{

  # programs.chromium.enable = true;

  programs.firefox = {
    enable = true;

    package = pkgs.firefox.override {
      cfg = {
        enableTridactylNative = true;
        speechSynthesisSupport = true;
      };
    };

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

  home.file.".mozilla/native-messaging-hosts".source = "${config.programs.firefox.package}/lib/mozilla/native-messaging-hosts";

  home.file.".tridactylrc".text = ''
    colors quake
    set editorcmd em
  '';
}
