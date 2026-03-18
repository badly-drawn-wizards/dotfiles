{ config, pkgs, lib, ... }:

let
  intellimacs = pkgs.fetchFromGitHub {
    owner = "MarcoIeni";
    repo = "intellimacs";
    rev = "cf9706cfeaf18e2247ee8f8c8289f1d196ce04b9";
    sha256 = "sha256-uANOwkA9EB3n1Kd+55420LJD7wrc4EDQ7z127HLvM2o=";
  };
  jdk-home = ".local/share/jetbrains-jdk";
  idea-jdk = pkgs.jetbrains.jdk.overrideAttrs (attrs: {
    passthru = attrs.passthru // { home = "${config.home.homeDirectory}/${jdk-home}"; };
  });
  idea = pkgs.jetbrains.idea.override {
    jdk = idea-jdk;
  };
  rider = pkgs.jetbrains.rider.override {
    jdk = idea-jdk;
  };
  fernflower-jar = "${idea}/idea-community/plugins/java-decompiler/lib/java-decompiler.jar";
  fernflower = pkgs.writeScriptBin "fernflower" ''
    #!/usr/bin/env bash
    ${idea-jdk}/bin/java -jar ${lib.escapeShellArg fernflower-jar} "$@"
  '';
in
{
  home.packages = [
    idea
    rider
    fernflower
  ];

  home.file = {
    ".ideavimrc".source = ./.ideavimrc;
    ".intellimacs".source = intellimacs;
    ${jdk-home}.source = pkgs.jetbrains.jdk.home;
  };
}
