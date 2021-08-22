{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    steam
    lutris

    # For proton
    python3
  ];

}
