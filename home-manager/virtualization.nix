{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    virt-manager
    virt-viewer
    quickemu
    vagrant
    docker
  ];
}
