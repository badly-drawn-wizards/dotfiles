{ config, lib, pkgs, ... }:
let
  inherit (config) icons;
  inherit (builtins)
    floor;
  inherit (pkgs) writeScript writeScriptBin jq pomo taskwarrior;
  inherit (pkgs.nix-colors.lib.conversions)
    hexToRGB;
  inherit (lib)
    mod length min max toHexString readFile
    escapeShellArg
    mapAttrsToList;
  inherit (lib.strings)
    removePrefix concatStrings concatMapStrings fixedWidthString;
  inherit (lib.lists)
    map mapAttrs genList zipListsWith reverseList elemAt;
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
  rgb-to-hex = concatMapStrings (v: fixedWidthString 2 "0" (toHexString v));
  color-mix-rgb = a: zipListsWith (c1: c2: floor ((1 - a) * c1 + a * c2));
  color-mix-hex = a: h1: h2:
    "#" + rgb-to-hex (color-mix-rgb a (hexToRGB (removePrefix "#" h1)) (hexToRGB (removePrefix "#" h2)));
  clamp = x: y: a: max x (min y a);
  lerp-list-with = f: xs: a:
    let
      a' = clamp 0 1 a;
      n = length xs - 1;
      x1 = floor (a' * n);
      x2 = min n (x1 + 1);
      b = a' - x1;
    in
    assert n > 0;
    f b (elemAt xs x1) (elemAt xs x2);
  arange1 = n: genList (x: 1.0 * x / (n - 1)) n;
  color-tiers-asc = map
    (lerp-list-with color-mix-hex [ theme.red theme.yellow theme.white ])
    (arange1 15);
  color-tiers-desc = reverseList color-tiers-asc;
  tag-open = tag: attrs:
    let
      body = concatStrings (mapAttrsToList (k: v: if v == null then "" else " ${k}=\"${v}\"") attrs);
    in
    ''<${tag}${body}>'';
  tag-close = tag: ''</${tag}>'';
  colored = text: color:
    "${tag-open "span" { inherit color; }}${text}${tag-close "span"}";

  playerctld-shift = "${pkgs.dbus}/bin/dbus-send --session --type=method_call --dest=org.mpris.MediaPlayer2.playerctld /org/mpris/MediaPlayer2 com.github.altdesktop.playerctld.Shift";
  mpris-config = {
    format = " | {player_icon}: {status_icon}";
    status-icons = {
      playing = icons.play;
      paused = icons.pause;
      stopped = icons.stop;
    };
    player-icons = {
      default = colored "" theme.purple;
      spotifyd = colored "" theme.green;
      firefox = colored "" theme.red;
    };
  };
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
              notification = "<span foreground='${theme.red}'><sup></sup></span>";
              none = "";
              dnd-notification = "<span foreground='${theme.red}'><sup></sup></span>";
              dnd-none = "";
              inhibited-notification = "<span foreground='${theme.red}'><sup></sup></span>";
              inhibited-none = "";
              dnd-inhibited-notification = "<span foreground='${theme.red}'><sup></sup></span>";
              dnd-inhibited-none = "";
            };
            return-type = "json";
            exec-if = "which swaync-client";
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
            format-icons = map (colored icons.battery) color-tiers-asc;
            interval = 2;
          };
          "cpu" = {
            format = "| {icon}: {usage} ";
            format-icons = map (colored icons.cpu) color-tiers-desc;
            interval = 2;
          };
          "memory" = {
            format = "| {icon}: {percentage} ";
            format-icons = map (colored icons.memory) color-tiers-desc;
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
    Service.Environment = [
      "PATH=${lib.makeBinPath [ pkgs.coreutils config.home.profileDirectory ]}"
    ];
  };
}
