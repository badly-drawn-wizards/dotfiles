{ config, lib, pkgs, ... }:
let
  inherit (config) icons;
  inherit (pkgs) writeScript writeScriptBin jq pomo taskwarrior;
  inherit (lib)
    readFile mapAttrsToList;
  inherit (lib.strings)
    concatStrings;
  inherit (lib.lists)
    map;

  colors = import ../../../lib/colors.nix { inherit lib pkgs; };
  inherit (colors) rgb-to-hex color-tiers-asc color-tiers-desc;

  theme = {
    background-darker = "#${rgb-to-hex [30 31 41]}";
    background = "#282a36";
    selection = "#44475a";
    foreground = "#f8f8f2";
    comment = "#6272a4";
    cyan = "#8be9fd";
    green = "#50fa7b";
    orange = "#ffb86c";
    pink = "#ff79c6";
    purple = "#bd93f9";
    red = "#ff5555";
    yellow = "#f1fa8c";
    white = "#ffffff";
  };

  color-tiers-asc-15 = color-tiers-asc theme 15;
  color-tiers-desc-15 = color-tiers-desc theme 15;
  tag-open = tag: attrs:
    let
      body = concatStrings (mapAttrsToList (k: v: if v == null then "" else " ${k}=\"${v}\"") attrs);
    in
    ''<${tag}${body}>'';
  tag-close = tag: ''</${tag}>'';
  colored = text: color:
    "${tag-open "span" { inherit color; }}${text}${tag-close "span"}";

in
{
  programs.waybar = {
    enable = true;
    systemd = {
      enable = true;
      target = "sway-session.target";
    };
    settings = [
      {
        layer = "top";
        position = "top";
        modules-left =
          [ "sway/workspaces" "sway/mode" "custom/taskwarrior" ];
        modules-center = [ "sway/window" ];
        modules-right =
          [
            "custom/rotate-on"
            "custom/rotate-off"

            "custom/notification"

            "idle_inhibitor"
            "custom/keyboard"
            "battery"
            "cpu"
            "memory"
            "disk"
            "clock"
            "tray"
          ];

        modules = {
          "sway/workspaces" = {
            disable-scroll = true;
            all-outputs = false;
          };

          "custom/rotate-on" = {
            format = "[ ";
            on-click = writeScript "sway-tablet" ''
              #!${pkgs.bash}/bin/bash
              ${pkgs.sway}/bin/swaymsg output eDP-1 transform 90 anticlockwise
            '';
          };
          "custom/rotate-off" = {
            format = "| ]";
            on-click = writeScript "sway-laptop" ''
              #!${pkgs.bash}/bin/bash
              ${pkgs.sway}/bin/swaymsg output eDP-1 transform normal
            '';
          };

          "sway/window".max-length = 70;

          "custom/notification" = {
            format = "{icon}";
            format-icons = {
              notification = "[<span foreground='${theme.red}'><sup>●</sup></span>]";
              none = "[]";
              dnd-notification = "[<span foreground='${theme.red}'><sup>●</sup></span>]";
              dnd-none = "[]";
              inhibited-notification = "[<span foreground='${theme.red}'><sup>●</sup></span>]";
              inhibited-none = "[]";
              dnd-inhibited-notification = "[<span foreground='${theme.red}'><sup>●</sup></span>]";
              dnd-inhibited-none = "[]";
            };
            return-type = "json";
            exec-if = "test -x ${pkgs.swaynotificationcenter}/bin/swaync-client";
            exec = "${pkgs.swaynotificationcenter}/bin/swaync-client -swb";
            on-click = "sleep 0.1 && ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
            on-click-right = "sleep 0.1 && ${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
            escape = true;
          };

          "idle_inhibitor" = {
            format = "[{icon} ";
            format-icons = {
              activated = icons.eye;
              deactivated = colored icons.eye theme.red;
            };
          };
          "custom/keyboard" = {
            format = "| {icon}]";
            tooltip = false;
            on-click = "${config.windowManager.kb-events.toggle}";
            exec-on-event = true;
            format-icons = [
              (colored icons.keyboard theme.red)
              icons.keyboard
            ];
            exec = writeScript "kb-event-get-percent" ''
              #!${pkgs.bash}/bin/bash
              ${config.windowManager.kb-events.get} | ${jq}/bin/jq -Rc 'if . == "enabled" then 100 else 0 end | {"text": "", "tooltip": "", "class": "", "percentage": .}'
            '';
            return-type = "json";
            interval = 5;
          };

          "battery" = {
            format = "[{icon}: {capacity} ";
            format-charging = "[${colored icons.battery theme.purple}: {capacity} ";
            format-full = "[${colored icons.battery theme.green}: {capacity} ";
            format-icons = map (colored icons.battery) color-tiers-asc-15;
            interval = 2;
          };
          "cpu" = {
            format = "| {icon}: {usage} ";
            format-icons = map (colored icons.cpu) color-tiers-desc-15;
            interval = 2;
          };
          "memory" = {
            format = "| {icon}: {percentage} ";
            format-icons = map (colored icons.memory) color-tiers-desc-15;
            interval = 2;
          };
          "disk" = {
            format = "| ${icons.storage}: {percentage_used}]";
            tooltip-format = "{used}/{total}";
          };

          "clock" = {
            format = "[${icons.clock}: {:%H:%M}]";
            tooltip-format = "[{:%y-%m-%d}]";
            interval = 2;
          };
        };
      }
    ];
    style = pkgs.writeTextFile {
      name = "waybar-css";
      text = lib.strings.concatLines (
        (lib.mapAttrsToList (k: v: "@define-color ${k} ${v};") theme) ++
        [ (readFile ./waybar.css) ]
      );
    };
  };

  systemd.user.services.waybar = {
    Unit = {
      After = [ "xdg-desktop-portal.service" ];
      Wants = [ "xdg-desktop-portal.service" ];
    };
    Service.Environment = [
      "PATH=${lib.makeBinPath [ pkgs.coreutils config.home.profileDirectory ]}"
    ];
  };
}
