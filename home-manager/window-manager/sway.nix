{ pkgs
, lib
, config
, ...
}:

with builtins;
with config.theme;
let
  inherit (config.wayland.windowManager.sway.config) modifier assigns;
  inherit (config.programs.doom-emacs) org-clock;
  inherit (lib) concatStrings;
  io = {
    monitor = "eDP-1";
    keyboard = "1:1:AT_Translated_Set_2_keyboard";
    touchpad = "type:touchpad";
  };

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

  mod = modifier;
  rofi-startup-workspaces =
    concatStrings (map (s: ''echo "${s}";'') (attrNames assigns));
  # Adapted from
  # https://blog.sarine.nl/2014/08/03/rofi-updates.html
  rofi-workspace = name: cmd:
    let
      cmd' = cmd "$@";
      script = ''
        #!/usr/bin/env sh
        if test -z $@
        then
          swaymsg -t get_workspaces | ${pkgs.jq}/bin/jq -r ".[].name" | { cat; ${rofi-startup-workspaces} } | sort | uniq
        else
          swaymsg "${cmd'}" >/dev/null
        fi
      '';
    in pkgs.writeScript name script;
  rofi-modi-cmd = config.programs.rofi.cmd.modi;

in
{
  options = with lib; with types; {
    windowManager = {
      startupPrograms = mkOption {
        type = listOf (either str attrs);
        default = [];
      };
      extraBinds = mkOption {
        type = attrsOf str;
        default = {};
      };

      kb-events = mkOption {
        type = attrsOf package;
        readOnly = true;
        default = rec {
          get = pkgs.writeScript "kb-events-get" ''
            #!${pkgs.bash}/bin/bash
            ${pkgs.sway}/bin/swaymsg -t get_inputs -r | jq -r '.[] | select (.identifier == "${io.keyboard}") | .libinput.send_events'
          '';

          do = pkgs.writeScript "kb-events-do" ''
            #!${pkgs.bash}/bin/bash
            ${pkgs.sway}/bin/swaymsg input ${io.keyboard} events "$@"
            ${pkgs.sway}/bin/swaymsg input ${io.touchpad} events $(${get})
          '';

          toggle = pkgs.writeScript "kb-events-do" ''
            #!${pkgs.bash}/bin/bash
            ${do} toggle enabled disabled
          '';
        };
      };
    };
  };

  config = {

    home.sessionVariables = {
      XDG_CONFIG_HOME=config.xdg.configHome;
      XDG_SESSION_TYPE="wayland";
      XDG_CURRENT_DESKTOP="sway";

      # Get sway to play nicely with IntelliJ
      _JAVA_AWT_WM_NONREPARENTING=1;
    };

    programs.rofi.modi = {
      "run" = null;
      "workspace" = rofi-workspace "rofi-workspace" (ws: "workspace ${ws}");
      "move" = rofi-workspace "rofi-move" (ws: "move window to workspace ${ws}");
    };

    wayland.windowManager.sway = {
      enable = true;

      systemd = {
        enable = true;
        xdgAutostart = true;
      };

      wrapperFeatures = {
        base = true;
        gtk = true;
      };

      extraSessionCommands = ''
        . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
      '';

      config = {
        terminal = "${pkgs.kitty}/bin/kitty";
        startup = map
          (command: if builtins.isString command then {
            inherit command;
          } else command)
          config.windowManager.startupPrograms;
        fonts = {
          names = [ "Font Awesome" "Fira Code" ];
        };
        modifier = "Mod4";
        menu = rofi-modi-cmd "run";
        focus = {
          mouseWarping = false;
          followMouse = false;
        };
        gaps = {
          inner = 5;
        };
        input = {
          "*" = {
            natural_scroll = "enabled";
            xkb_layout = "us";
            xkb_options = "caps:swapescape";
          };
          "type:touch" = {
            map_to_output = io.monitor;
          };
          "type:tablet_tool" = {
            map_to_output = io.monitor;
          };
          "type:touchpad" = {
            map_to_output = io.monitor;
          };
        };
        output = {
          ${io.monitor} = {
            scale = "1.5";
          #   mode = "3840x2168";
          };
        };
        assigns = {
          " web" = [
            { app_id = "^firefox$"; }
          ];
          " read" = [
            { app_id = "^calibre-gui$"; }
            { class = "^MuPdf$"; }
            { class = "^okular$"; }
          ];
          " chat" = [
            { class = "^discord$"; }
            { class = "^Riot$"; }
            { class = "^Slack$"; }
            { app_id = "^thunderbird$"; }
          ];
          " write" = [
            { class = "^Write$"; }
            { app_id = "^xournalpp"; }
          ];
        };

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

            "${mod}+Shift+r" = "reload";
            "${mod}+i" = "exec kitty nvim";
            "${mod}+Shift+i" = "exec em";

            "${mod}+o" = "exec ${rofi-modi-cmd "workspace"}";
            "${mod}+Shift+o" = "exec ${rofi-modi-cmd "move"}";

            "${mod}+c" = "exec ${org-clock} toggle-last-clock";
            "${mod}+Shift+c" = "exec ${org-clock} recent-clock";
            "${mod}+t" = "exec ${pkgs.gnome.nautilus}/bin/nautilus";
            "${mod}+Print" = ''exec ${pkgs.grim}/bin/grim -t png -g "$(${pkgs.slurp}/bin/slurp)" ${config.home.homeDirectory}/screenshots/$(date +%Y-%m-%d_%H-%m-%s).png'';
            "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +10%";
            "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%-";
            "XF86Launch1" = "exec ${config.windowManager.kb-events.toggle}";
          };

        in lib.mkOptionDefault (
          focusKeybinds //
          moveKeybinds //
          otherKeybinds //
          config.windowManager.extraBinds
        );
        colors = {
          focused = mkColorSet color5 color0;
          focusedInactive = mkColorSet color0 color12;
          unfocused = mkColorSet color0 color12;
        };
        bars = [];
      };
    };
  };
}
