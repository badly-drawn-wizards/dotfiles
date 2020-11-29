{ pkgs
, config
, ...
}:
with builtins;
with config.theme;
let
  inherit (pkgs) lib;
  mkColorSet = bg: txt: { 
    border = "#${bg}"; 
    childBorder = "#${bg}"; 
    background = "#${bg}"; 
    indicator = "#${bg}"; 
    text = "#${txt}"; 
  };
  mkBarColorSet = bg: txt: {
    border = "#${bg}"; 
    background = "#${bg}"; 
    text = "#${txt}"; 
  };
  mod = "Mod4";

  rofi-run = modi:
    ''${pkgs.rofi}/bin/rofi -show ${modi}'';

  # TODO switch this over to rofi
  rename-workspace = ''i3-input -F "rename workspace to %s"'';

  assigns = {
    " web" = [
      { class = "^Firefox$"; }
    ];
    " read" = [
      { class = "^calibre$"; }
      { class = "^MuPdf$"; }
      { class = "^okular$"; }
    ];
    " chat" = [
      { class = "^discord$"; }
      { class = "^Riot$"; }
      { class = "^Slack$"; }
    ];
    " write" = [
      { class = "^Write$"; }
    ];
  };

  # Adapted from
  # https://blog.sarine.nl/2014/08/03/rofi-updates.html
  rofi-startup-workspaces = lib.concatStrings (map (s: ''echo "${s}";'') (attrNames assigns));
  rofi-workspace-bin = name: cmd:
    let
      cmd' = cmd "$@";
      script = ''
        #!/bin/sh
        if test -z $@
        then
          swaymsg -t get_workspaces | ${pkgs.jq}/bin/jq -r ".[].name" | { cat; ${rofi-startup-workspaces} } | sort | uniq
        else
          swaymsg "${cmd'}" >/dev/null
        fi
      '';
    in pkgs.writeScriptBin name script;
  rofi-workspace-cmd = name: cmd: ''${rofi-workspace-bin name cmd}/bin/${name}'';

  rofi-workspace = rofi-workspace-cmd "rofi-workspace" (ws: "workspace ${ws}");
  rofi-move = rofi-workspace-cmd "rofi-move" (ws: "move window to workspace ${ws}");

in {
  options = with lib; with types; {
    window-manager = {
      startupPrograms = mkOption {
        type = listOf str;
        default = [];
      };
      extraBinds = mkOption {
        type = attrsOf str;
        default = {};
      };
    };
  };
  config = {
    programs.rofi = {
      enable = true;
      font = "Fira Code 8";
      terminal = "${pkgs.rxvt-unicode}/bin/urxvt";
      extraConfig = ''
        rofi.modi: run#window#workspace:${rofi-workspace}#move:${rofi-move}
        rofi.kb-row-tab:
        rofi.kb-remove-to-eol:
        rofi.kb-accept-entry: Return
        rofi.kb-mode-next: Tab
        rofi.kb-mode-previous: Shift+Tab
        rofi.kb-row-up: Control+k
        rofi.kb-row-down: Control+j
      '';
    };

    programs.waybar = {
      enable = true;
      settings = [
        {
          layer = "top";
          position = "top";
          modules-left = [ "sway/workspaces" "sway/mode" ];
          modules-center = [];
          modules-right = ["network" "pulseaudio" "battery" "clock" "tray"];
        }
      ];
      style = ''
        * {
            border: none;
            border-radius: 0;
            /* `otf-font-awesome` is required to be installed for icons */
            font-family: Fira Code, sans-serif;
            font-size: 13px;
            min-height: 0;
        }

        window#waybar {
            background-color: rgba(43, 48, 59, 0.5);
            border-bottom: 3px solid rgba(100, 114, 125, 0.5);
            color: #ffffff;
            transition-property: background-color;
            transition-duration: .5s;
        }

        window#waybar.hidden {
            opacity: 0.2;
        }

        /*
        window#waybar.empty {
            background-color: transparent;
        }
        window#waybar.solo {
            background-color: #FFFFFF;
        }
        */

        window#waybar.termite {
            background-color: #3F3F3F;
        }

        window#waybar.chromium {
            background-color: #000000;
            border: none;
        }

        #workspaces button {
            padding: 0 5px;
            background-color: transparent;
            color: #ffffff;
            /* Use box-shadow instead of border so the text isn't offset */
            box-shadow: inset 0 -3px transparent;
        }

        /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
        #workspaces button:hover {
            background: rgba(0, 0, 0, 0.2);
            box-shadow: inset 0 -3px #ffffff;
        }

        #workspaces button.focused {
            background-color: #64727D;
            box-shadow: inset 0 -3px #ffffff;
        }

        #workspaces button.urgent {
            background-color: #eb4d4b;
        }

        #mode {
            background-color: #64727D;
            border-bottom: 3px solid #ffffff;
        }

        #clock,
        #battery,
        #cpu,
        #memory,
        #temperature,
        #backlight,
        #network,
        #pulseaudio,
        #custom-media,
        #tray,
        #mode,
        #idle_inhibitor,
        #mpd {
            padding: 0 10px;
            margin: 0 4px;
            color: #ffffff;
        }

        #clock {
            background-color: #64727D;
        }

        #battery {
            background-color: #ffffff;
            color: #000000;
        }

        #battery.charging {
            color: #ffffff;
            background-color: #26A65B;
        }

        @keyframes blink {
            to {
                background-color: #ffffff;
                color: #000000;
            }
        }

        #battery.critical:not(.charging) {
            background-color: #f53c3c;
            color: #ffffff;
            animation-name: blink;
            animation-duration: 0.5s;
            animation-timing-function: linear;
            animation-iteration-count: infinite;
            animation-direction: alternate;
        }

        label:focus {
            background-color: #000000;
        }

        #cpu {
            background-color: #2ecc71;
            color: #000000;
        }

        #memory {
            background-color: #9b59b6;
        }

        #network {
            background-color: #2980b9;
        }

        #network.disconnected {
            background-color: #f53c3c;
        }

        #pulseaudio {
            background-color: #f1c40f;
            color: #000000;
        }

        #pulseaudio.muted {
            background-color: #90b1b1;
            color: #2a5c45;
        }

        #temperature {
            background-color: #f0932b;
        }

        #temperature.critical {
            background-color: #eb4d4b;
        }

        #tray {
            background-color: #2980b9;
        }

        #idle_inhibitor {
            background-color: #2d3436;
        }

        #idle_inhibitor.activated {
            background-color: #ecf0f1;
            color: #2d3436;
        }

        #mpd {
            background-color: #66cc99;
            color: #2a5c45;
        }

        #mpd.disconnected {
            background-color: #f53c3c;
        }

        #mpd.stopped {
            background-color: #90b1b1;
        }

        #mpd.paused {
            background-color: #51a37a;
        }

        #language {
            background: #00b093;
            color: #740864;
            padding: 0 5px;
            margin: 0 5px;
            min-width: 16px;
        }
      '';
    };

    wayland.windowManager.sway = {
      enable = true;

      extraConfig = ''
      input 1267:10741:ELAN2514:00_04F3:29F5 map_to_output eDP-1
      '';

      config = {
        startup = map
          (command: {
            inherit command;
          })
          config.window-manager.startupPrograms;
        fonts = [ "Font Awesome 8" "Fira Code 8" ];
        modifier = mod;
        menu = rofi-run "run";
        focus = {
          mouseWarping = false;
          followMouse = false;
        };
        gaps = {
          inner = 5;
        };
        input = {
          "1:1:AT_Translated_Set_2_keyboard" = {
            xkb_layout = "us";
            xkb_options = "caps:swapescape";
          };
        };
        output = {
          "eDP-1" = {
            scale = "2";
            bg = "$HOME/backgrounds/megumin.jpg fill";
            mode = "3840x2168";
          };
        };
        inherit assigns;
        keybindings = with lib.attrsets; let

          # Usual i3 movement
          dirMap = {
            j = "left";
            k = "down";
            l = "up";
            semicolon = "right";
          };
          focusKeybinds = mapAttrs' (key: dir: nameValuePair "${mod}+${key}" "focus ${dir}") dirMap;
          moveKeybinds = mapAttrs' (key: dir: nameValuePair "${mod}+Shift+${key}" "move ${dir}") dirMap;

          otherKeybinds = {
            "${mod}+Shift+minus" = "move scratchpad";
            "${mod}+minus" = "scratchpad show";

            "${mod}+r" = "exec ${rename-workspace}";
            "${mod}+Shift+r" = "reload";
            "${mod}+i" = "exec em";
            "${mod}+o" = "exec ${rofi-run "workspace"}";
            "${mod}+Shift+o" = "exec ${rofi-run "move"}";
            "${mod}+p" = "exec ${rofi-run "window"}";

          };

        in lib.mkOptionDefault (
          focusKeybinds //
          moveKeybinds //
          otherKeybinds //
          config.window-manager.extraBinds
        );
        colors = {
          focused = mkColorSet base02 base05;
          focusedInactive = mkColorSet base00 base0E;
          unfocused = mkColorSet base00 base0E;
        };
        bars = [{
          position = "top";
          command = "${pkgs.sway}/bin/swaybar";
          # command = "${pkgs.waybar}/bin/waybar";
          colors = {
            background = "#${base00}";
            statusline = "#${base04}";
            separator = "#${base02}";
            focusedWorkspace = mkBarColorSet base09 dark00;
            inactiveWorkspace = mkBarColorSet dark00 base09;
          };
        }];
      };
    };
  };
}
