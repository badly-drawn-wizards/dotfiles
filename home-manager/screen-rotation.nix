{ config, lib, pkgs, ... }:

let
  inherit (config.windowManager) io;
  dbus-send = "${pkgs.dbus}/bin/dbus-send";
  iio-rotate = pkgs.writeScriptBin "iio-rotate" ''
    #!${pkgs.bash}/bin/bash
    ${dbus-send} --system --print-reply \
      --dest=net.hadess.SensorProxy \
      /net/hadess/SensorProxy \
      net.hadess.SensorProxy.ClaimAccelerometer

    orientation=$(${dbus-send} --system --print-reply \
      --dest=net.hadess.SensorProxy \
      /net/hadess/SensorProxy \
      org.freedesktop.DBus.Properties.Get \
      string:net.hadess.SensorProxy string:AccelerometerOrientation \
      2>/dev/null | ${pkgs.gnused}/bin/sed -n 's/.*string "\(.*\)"/\1/p')

    ${dbus-send} --system --print-reply \
      --dest=net.hadess.SensorProxy \
      /net/hadess/SensorProxy \
      net.hadess.SensorProxy.ReleaseAccelerometer

    case "$orientation" in
      normal)        transform="normal" ;;
      right-up)      transform="90" ;;
      bottom-up)     transform="180" ;;
      left-up)       transform="270" ;;
      *)             transform="normal" ;;
    esac

    ${pkgs.sway}/bin/swaymsg output ${io.monitor} transform "$transform"
  '';
in
{
  home.packages = [ iio-rotate ];
}
