{ pkgs
, config
, lib
, ...
}:
let
  cpu = "";
  memory = "";
  storage = "";
  clock = "";
  calendar = "";
  charging = "";
  discharging = "";
  full = "";
  battery = "";
  thermometer = "";
in
{
  programs.i3status = {
    enable = true;
    enableDefault = false;
    general = {
      separator = " / ";
    };
    modules = {
      "battery all" = {
        position = 1;
        settings = {
          format = "${battery} %percentage (%remaining %status) ";
          integer_battery_capacity = true;
          status_chr = charging;
          status_bat = discharging;
          status_full = full;
          low_threshold = 10;
        };
      };
      load = {
        position = 2;
        settings = { format = " ${cpu} %1min "; };
      };
      "cpu_temperature 0" = {
        position = 3;
        settings = {
          format = " ${thermometer} %degrees°C ";
        };
      };

      memory = {
        position = 4;
        settings = {
          format = " ${memory} %used + %available ";
        };
      };

      "disk /" = {
        position = 5;
        settings = { format = " ${storage} %avail / %total "; };
      };

      "tztime local" = {
        position = 6;
        settings = { format = " ${clock} %H:%M:%S ${calendar} %d/%m"; };
      };
    };
  };
}
