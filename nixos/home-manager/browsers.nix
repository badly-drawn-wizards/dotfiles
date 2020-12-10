{ pkgs
, config
, lib
, ...
}@args:
{

  programs.chromium.enable = true;

  programs.firefox = {
    enable = true;

    package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
      # extraPolicies = {
      #   ExtensionSettings = {};
      # };
    };

    extensions = [];
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
