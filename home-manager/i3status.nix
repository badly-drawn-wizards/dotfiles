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
  keyboard = "";
in
{
  programs.i3status = {
    enable = true;
    enableDefault = false;
    # general = {
    #   separator = " / ";
    # };
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

      "external_script disk" = {
        position = 5;
        settings = {
          format = " ${storage} {output}";
          script_path = ''${pkgs.writeScriptBin "i3status-get-disk" ''
            used=$(df -h --output=used / | tail -n1 | tr -d ' ')
            size=$(df -h --output=size / | tail -n1 | tr -d ' ')
            echo "$used / $size"
          ''}/bin/i3status-get-disk'';
          cache_timeout = 60;
          "on_click 1" = "exec ${pkgs.gnome.nautilus}/bin/nautilus";
        };
      };
      "tztime local" = {
        position = 6;
        settings = { format = " ${clock} %H:%M:%S ${calendar} %d/%m"; };
      };
      "external_script keyboard" = {
        position = 7;
        settings = {
          format = "{output}";
          script_path = ''${pkgs.writeScriptBin "keyboard-indicator" ''
          echo "${keyboard}"
          ''}/bin/keyboard-indicator'';
          cache_timeout = 60;
          "on_click 1" = "exec ${config.onscreen-keyboard.togglePackage}/bin/toggleKeyboard";
        };
      };

    };
  };
}
