{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    python3Packages.python-lsp-server
    (python3.withPackages (ps: with ps; [
      jupyterlab
      numpy matplotlib
      torch transformers
      ipython
      python-uinput
    ]))
  ];
}
