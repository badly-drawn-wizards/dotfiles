{ config, pkgs, ... }:

let
  jdk-home = ".local/share/jetbrains-jdk";
  jdk = pkgs.jetbrains.jdk.overrideAttrs (attrs: {
    passthru = attrs.passthru // { home = "${config.home.homeDirectory}/${jdk-home}"; };
  });
in
{
  home.packages = with pkgs; [
    jdk
    (jetbrains.idea-community.override {
      inherit jdk;
    })
  ];

  home.file = {
    ".ideavimrc".source = ./.ideavimrc;
    ${jdk-home}.source = pkgs.jetbrains.jdk.home;
  };
}
