{ pkgs
, lib
, config
, ...
}:

let
  inherit (config.wayland.windowManager.sway.config) modifier assigns;
  inherit (config.windowManager) io;
  inherit (lib) concatStrings;

  defaultScale = "1.5";

  mkColorSet = bg: txt: {
    border = "#${bg}";
    childBorder = "#${bg}";
    background = "#${bg}";
    indicator = "#${bg}";
    text = "#${txt}";
  };

  mod = modifier;
  rofi-startup-workspaces =
    concatStrings (map (s: ''echo "${s}";'') (builtins.attrNames assigns));
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
    in
    pkgs.writeScript name script;
  rofi-modi-cmd = config.programs.rofi.cmd.modi;
  ipc = "${config.programs.noctalia.package}/bin/noctalia msg";
  sway-other-monitor = pkgs.writeScript "sway-other-monitor" ''
    #!${pkgs.bash}/bin/bash
    swaymsg -t get_outputs | jq -r '. | sort_by(.focused) | .[0].name'
  '';

  toggle-edp-scale = pkgs.writeScriptBin "toggle-edp-scale" ''
    #!${pkgs.bash}/bin/bash
    current_scale=$(swaymsg -t get_outputs | ${pkgs.jq}/bin/jq -r '.[] | select(.name == "${io.monitor}") | .scale')
    if [ "$current_scale" = "${defaultScale}" ]; then
      swaymsg output ${io.monitor} scale 1
    else
      swaymsg output ${io.monitor} scale ${defaultScale}
    fi
  '';

in
{
  options = with lib; with types; {
    windowManager = {
      startupPrograms = mkOption {
        type = listOf (either str attrs);
        default = [ ];
      };
      extraBinds = mkOption {
        type = attrsOf str;
        default = { };
      };

      io = mkOption {
        default = {
          monitor = "eDP-1";
          external-monitor = "DP-1";
          keyboard = "1:1:AT_Translated_Set_2_keyboard";
          touchpad = "type:touchpad";
        };
      };
    };
  };

  config = {

    home.sessionVariables = {
      XDG_CONFIG_HOME = config.xdg.configHome;
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "sway";
      WLR_RENDERER = "vulkan";

      # Get sway to play nicely with IntelliJ
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };

    programs.rofi.modi = {
      "workspace" = rofi-workspace "rofi-workspace" (ws: "workspace ${ws}");
      "move" = rofi-workspace "rofi-move" (ws: "move window to workspace ${ws}");
    };

    wayland.windowManager.sway = {
      enable = true;

      systemd = {
        enable = true;
        variables = [ "--all" ];
        xdgAutostart = true;
      };

      wrapperFeatures = {
        base = true;
        gtk = true;
      };

      config = {
        terminal = "${pkgs.kitty}/bin/kitty";
        startup = (map
          (command:
            if builtins.isString command then {
              inherit command;
            } else command)
          config.windowManager.startupPrograms);
        fonts = {
          names = [ "Font Awesome" "Fira Code" ];
        };
        modifier = "Mod4";
        menu = "${ipc} panel-toggle launcher";
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
        };
        output = {
          ${io.monitor} = {
            scale = defaultScale;
            adaptive_sync = "on";
            render_bit_depth = "10";
            hdr = "off";
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
            { app_id = "^element$"; }
            { app_id = "^Zulip$"; }
            { app_id = "^thunderbird$"; }
            { app_id = "^whatsapp-for-linux$"; }
            { app_id = "^teams-for-linux$"; }
            { app_id = "^org.gnome.Evolution$"; }
          ];
          " write" = [
            { class = "^Write$"; }
            { app_id = "^xournalpp"; }
          ];
          " music" = [
            { class = "^Spotify$"; }
          ];
          " pass" = [
            { floating = "^user_on$"; app_id = "^org.keepassxc.KeePassXC$"; }
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

            "${mod}+Tab" = "workspace back_and_forth";
            "${mod}+o" = "exec ${rofi-modi-cmd "workspace"}";
            "${mod}+Shift+o" = "exec ${rofi-modi-cmd "move"}";

            "${mod}+p" = "exec bash -c \"${pkgs.sway}/bin/swaymsg focus output $(${sway-other-monitor})\"";
            "${mod}+Shift+p" = "exec bash -c \"${pkgs.sway}/bin/swaymsg move workspace to output $(${sway-other-monitor})\"";

            "${mod}+t" = "exec ${pkgs.nautilus}/bin/nautilus";
            "${mod}+Shift+a" = "exec ${ipc} session lock";
            "${mod}+grave" = "exec ${ipc} notifications-clear-active";
            "${mod}+Shift+n" = "exec ${ipc} notification-dnd-toggle";
            "${mod}+Shift+s" = "exec ${ipc} screenshot-region";
            "${mod}+b" = "exec ${ipc} panel-toggle wallpaper";
            "--locked XF86MonBrightnessUp" = "exec ${ipc} brightness-up";
            "--locked XF86MonBrightnessDown" = "exec ${ipc} brightness-down";
            "--locked XF86AudioRaiseVolume" = "exec ${ipc} volume-up";
            "--locked XF86AudioLowerVolume" = "exec ${ipc} volume-down";
            "--locked XF86AudioMute" = "exec ${ipc} volume-mute";
            "XF86Launch3" = "exec iio-rotate";
            "${mod}+bracketright" = "exec ${toggle-edp-scale}/bin/toggle-edp-scale";
          };

        in
        lib.mkOptionDefault (
          focusKeybinds //
          moveKeybinds //
          otherKeybinds //
          config.windowManager.extraBinds
        );
        colors = with config.theme; {
          focused = mkColorSet color5 color0;
          focusedInactive = mkColorSet color0 color12;
          unfocused = mkColorSet color0 color12;
        };
        bars = [ ];
      };
    };
  };
}
