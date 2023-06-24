{ config, lib, pkgs, ... }:
let
  inherit (config) icons;
  inherit (builtins)
    floor;
  inherit (pkgs) writeScript writeScriptBin jq;
  inherit (pkgs.nix-colors.lib.conversions)
    hexToRGB;
  inherit (lib)
    mod length min max toHexString readFile
    escapeShellArg escape
    mapAttrsToList;
  inherit (lib.strings)
    removePrefix concatStrings concatMapStrings fixedWidthString;
  inherit (lib.lists)
    map genList zipListsWith reverseList elemAt;
  inherit (config.programs.doom-emacs) org-clock org-agenda;
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
  color-mix-rgb = a: zipListsWith (c1: c2: floor ((1 - a)*c1 + a*c2));
  color-mix-hex = a: h1: h2:
    "#" + rgb-to-hex (color-mix-rgb a (hexToRGB (removePrefix "#" h1)) (hexToRGB (removePrefix "#" h2)));
  clamp = x: y: a: max x (min y a);
  lerp-list-with = f: xs: a: let
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
    (arange1 30);
  color-tiers-desc = reverseList color-tiers-asc;
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
      settings = [
        {
          layer = "top";
          position = "top";
          modules-left =
            ["sway/workspaces" "sway/mode" "custom/org-clock" ];
          modules-center = [ "sway/window" ];
          modules-right =
            [
              "idle_inhibitor"
              "custom/osk"
              "battery"
              "cpu"
              "memory"
              "disk"
              "custom/org-agenda"
              "clock"
              "tray"
            ];

          modules = {
            "sway/workspaces" = {
              disable-scroll = true;
              all-outputs = true;
            };
            "custom/org-clock" = {
              max-length = 100;
              exec = org-clock;
              return-type = "json";
              interval = 2;
              exec-on-event = true;
              on-click = "${org-clock} toggle-last-clock";
              on-click-right = "${org-clock} recent-clock";
            };

            "sway/window".max-length = 70;

            "battery" = {
              bat = "BAT0";
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

            "custom/org-agenda" = {
              format = "[${icons.calendar} ";
              exec = org-agenda;
              return-type = "json";
              interval = 30;
            };
            "clock" = {
              format = "| ${icons.clock}: {:%H:%M}]";
              tooltip-format = "[{:%y-%m-%d}]";
              interval = 2;
            };
            "idle_inhibitor" = {
              format = "[{icon} ";
              format-icons = {
                activated = icons.eye;
                deactivated = icons.eye-slash;
              };
            };
            "custom/osk" = {
              format = "| ${icons.keyboard}]";
              tooltip = false;
              on-click = "${config.onscreen-keyboard.toggle}";
            };
          };
        }
      ];
      style = pkgs.writeTextFile {
        name = "waybar-css";
        text = lib.strings.concatLines (
            (lib.mapAttrsToList (k: v: "@define-color ${k} ${v};") theme) ++
            [(readFile ./waybar.css)]
        );
      };
    };
}