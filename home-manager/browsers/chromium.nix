{ config, lib, pkgs, ... }:

{
  # Ungoogled Chromium, beside Firefox: a clean Chromium for browser tests and
  # local pages. `lib.getExe` follows the package's own `meta.mainProgram`, so the
  # wrapper does not hard-code a binary name that a nixpkgs update could change.
  home.packages = [
    pkgs.ungoogled-chromium
    (pkgs.writeShellScriptBin "chromium" ''
      exec ${lib.getExe pkgs.ungoogled-chromium} "$@"
    '')
  ];
}
