{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.logitune ];

  # Ships 71-logitune.rules (uaccess for Logitech hidraw + uinput).
  services.udev.packages = [ pkgs.logitune ];

  # Load uinput so udev has a misc/uinput device to tag for keystroke injection.
  boot.kernelModules = [ "uinput" ];
}
