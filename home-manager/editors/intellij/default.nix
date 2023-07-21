{ config, pkgs, lib, ... }:

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
  idea-community = pkgs.jetbrains.idea-community.override {
    jdk = idea-jdk;
  };
  fernflower-jar = "${idea-community}/idea-community/plugins/java-decompiler/lib/java-decompiler.jar";
  fernflower = pkgs.writeScriptBin "fernflower" ''
  #!/usr/bin/env bash
  ${idea-jdk}/bin/java -jar ${lib.escapeShellArg fernflower-jar} "$@"
  '';
in
{
  home.packages = with pkgs; [
    idea-community
    fernflower
  ];

  home.file = {
    ".ideavimrc".source = ./.ideavimrc;
    ".intellimacs".source = intellimacs;
    ${jdk-home}.source = pkgs.jetbrains.jdk.home;
  };
}
