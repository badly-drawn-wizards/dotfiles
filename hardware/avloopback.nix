{ config, lib, pkgs, ... }:

{
  boot = {
    kernelModules = [
      "v4l2loopback"
      "snd-aloop"
    ];

    extraModprobeConfig = ''
    options v4l2loopback exclusive_caps=1 card_label="Screen"
    '';
  };
}
