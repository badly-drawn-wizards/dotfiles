{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    (python3.withPackages (ps: with ps; [
      jupyterlab
      numpy
      matplotlib
      sympy
      ipython
      fastapi
    ]))
  ];
}
