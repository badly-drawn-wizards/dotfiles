{ pkgs
, config
, lib
, ...
}@args:
{
  programs.firefox = {
    enable = true;
    profiles."default" = {
      path = "lly2q038.default";
      userChrome = ''
      @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul"); /* set default namespace to XUL */
      #TabsToolbar {visibility: collapse;}
    '';
    };
  };

  home.sessionVariables.MOZ_USE_XINPUT2 = "1";

  home.packages = with pkgs; [
    tridactyl-native
  ];
}
