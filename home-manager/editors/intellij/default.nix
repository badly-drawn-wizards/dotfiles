{ config, pkgs, ... }:

let
  intellimacs = pkgs.fetchFromGitHub {
    owner = "MarcoIeni";
    repo = "intellimacs";
    rev = "116e566bafb4c7fe9a2962a746281053e59b1f49";
    sha256 = "sha256-Aomn1sS2ZlruWy8UMKkPLn1bOX3kLThRTgLgzFUo/yk=";
  };
  jdk-home = ".local/share/jetbrains-jdk";
  idea-jdk = pkgs.jetbrains.jdk.overrideAttrs (attrs: {
    passthru = attrs.passthru // { home = "${config.home.homeDirectory}/${jdk-home}"; };
  });
in
{
  home.packages = with pkgs; [
    (jetbrains.idea-community.override {
      jdk = idea-jdk;
    })
  ];

  home.file = {
    ".ideavimrc".source = ./.ideavimrc;
    ".intellimacs".source = intellimacs;
    ${jdk-home}.source = pkgs.jetbrains.jdk.home;
  };
}
