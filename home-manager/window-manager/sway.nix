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
  sway-other-monitor = pkgs.writeScript "sway-other-monitor" ''
    #!${pkgs.bash}/bin/bash
    swaymsg -t get_outputs | jq -r '. | sort_by(.focused) | .[0].name'
  '';

  sway-session-init = pkgs.writeScriptBin "sway-session-init" ''
    #!${pkgs.bash}/bin/bash
    # Set SSH_AUTH_SOCK to gcr-ssh-agent socket
    export SSH_AUTH_SOCK="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/gcr/ssh"

    # Update dbus activation environment with all variables
    ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd \
      WAYLAND_DISPLAY \
      XDG_CURRENT_DESKTOP=sway \
      SSH_AUTH_SOCK
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
      XDG_CONFIG_HOME = config.xdg.configHome;
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "sway";

      # Get sway to play nicely with IntelliJ
      _JAVA_AWT_WM_NONREPARENTING = 1;
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

      # extraSessionCommands = ''
      #   . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
      # '';

      config = {
        terminal = "${pkgs.kitty}/bin/kitty";
        startup = [
          { command = "${sway-session-init}/bin/sway-session-init"; }
        ] ++ (map
          (command:
            if builtins.isString command then {
              inherit command;
            } else command)
          config.windowManager.startupPrograms);
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
        };
        output = {
          ${io.monitor} = {
            scale = defaultScale;
            mode = "2560x1600";
            adaptive_sync = "on";
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
            { class = "^Element$"; }
            { class = "^Zulip$"; }
            { class = "^Mattermost$"; }
            { app_id = "^thunderbird$"; }
            { app_id = "^whatsapp-for-linux$"; }
            { app_id = "^teams-for-linux$"; }
          ];
          " write" = [
            { class = "^Write$"; }
            { app_id = "^xournalpp"; }
          ];
          " music" = [
            { class = "^Spotify$"; }
          ];
          " pass" = [
            { app_id = "^org.keepassxc.KeePassXC$"; }
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
            "${mod}+i" = "exec kitty reloadable-nvim";
            "${mod}+Shift+i" = "exec em";

            "${mod}+Tab" = "workspace back_and_forth";
            "${mod}+o" = "exec ${rofi-modi-cmd "workspace"}";
            "${mod}+Shift+o" = "exec ${rofi-modi-cmd "move"}";

            "${mod}+p" = "exec bash -c \"${pkgs.sway}/bin/swaymsg focus output $(${sway-other-monitor})\"";
            "${mod}+Shift+p" = "exec bash -c \"${pkgs.sway}/bin/swaymsg move workspace to output $(${sway-other-monitor})\"";

            "${mod}+t" = "exec ${pkgs.nautilus}/bin/nautilus";
            "${mod}+Shift+s" = "exec ${pkgs.screenshot}/bin/screenshot";
            "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +10%";
            "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%-";
            "XF86AudioRaiseVolume" = "exec ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+";
            "XF86AudioLowerVolume" = "exec ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
            "XF86AudioMute" = "exec ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
            "XF86Launch1" = "exec ${config.windowManager.kb-events.toggle}";
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
