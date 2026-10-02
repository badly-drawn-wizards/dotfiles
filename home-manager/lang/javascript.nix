{ pkgs, lib, ... }:

let
  # Prebuilt native addons (e.g. keytar) dlopen these; nixpkgs node's loader
  # only finds them through LD_LIBRARY_PATH, not the profile or nix-ld.
  nativeAddonLibs = lib.makeLibraryPath [
    pkgs.libsecret
    pkgs.glib
    pkgs.stdenv.cc.cc.lib
  ];

  azure-devops-mcp = pkgs.writeShellScriptBin "azure-devops-mcp" ''
    export LD_LIBRARY_PATH="${nativeAddonLibs}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    exec ${pkgs.nodejs_latest}/bin/npx -y @azure-devops/mcp "$@"
  '';
in
{
  home.packages = [
    pkgs.nodejs_latest
    azure-devops-mcp
  ];
}
