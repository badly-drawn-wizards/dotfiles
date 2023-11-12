{ pkgs, ... }:

{
  home.packages = [
    (
      pkgs.sage.override {
        extraPythonPackages = ps: with ps; [ ps.jupyterlab ];
        requireSageTests = false;
      }
    )
  ];
}
