{ pkgs
, lib
, config
, ...
}:

with builtins;
with config.theme;
let
  io = {
    keyboard = "1:1:AT_Translated_Set_2_keyboard";
    stylus = "1267:10741:ELAN2514:00_04F3:29F5_Stylus";
    touchpad = "1267:10741:ELAN2514:00_04F3:29F5";
    monitor = "eDP-1";
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
  mod = "Mod4";
  terminal = "${pkgs.kitty}/bin/kitty";

  rofi-run = modi:
    ''${pkgs.rofi}/bin/rofi -matching fuzzy -show ${modi}'';

  # TODO switch this over to rofi
  rename-workspace = ''i3-input -F "rename workspace to %s"'';

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

in
{
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

    home.sessionVariables = {
      XDG_SESSION_TYPE="wayland";
      XDG_CURRENT_DESKTOP="sway";

      # Get sway to play nicely with IntelliJ
      _JAVA_AWT_WM_NONREPARENTING=1;
    };

    home.file.".config/rofi/config.rasi".text = ''
      @theme "${pkgs.dracula-rofi-theme}/theme/config2.rasi"
      * {
        font: "${config.font} ${builtins.toString config.fontSize}";
      }
      configuration {
        modi: "run,window,workspace:${rofi-workspace},move:${rofi-move}";
        kb-row-tab: "";
        kb-remove-to-eol: "";
        kb-accept-entry: "Return";
        kb-mode-next: "Tab";
        kb-mode-previous: "Shift+Tab";
        kb-row-up: "Control+k";
        kb-row-down: "Control+j";
      }
    '';
    home.packages = with pkgs; [ rofi ];

    window-manager.startupPrograms = [
      # "${pkgs.autotiling}/bin/autotiling"
    ];

    wayland.windowManager.sway = {
      enable = true;


      extraSessionCommands = ''
      . ${config.home.homeDirectory}/.profile
      '';

      config = {
        inherit terminal;
        startup = map
          (command: {
            inherit command;
          })
          config.window-manager.startupPrograms;
        fonts = {
          names = [ "Font Awesome" "Fira Code" ];
        };
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
          ${io.keyboard} = {
            xkb_layout = "us";
            xkb_options = "caps:swapescape";
            repeat_rate = "0";
          };
          ${io.touchpad} = {
            map_to_output = io.monitor;
          };
          ${io.stylus} = {
            map_to_output = io.monitor;
          };
        };
        output = {
          ${io.monitor} = {
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
            "${mod}+m" = "exec mpv-paste";
            "${mod}+t" = "exec thunar";

            "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +10%";
            "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%-";
          };

        in lib.mkOptionDefault (
          focusKeybinds //
          moveKeybinds //
          otherKeybinds //
          config.window-manager.extraBinds
        );
        colors = {
          focused = mkColorSet color5 color0;
          focusedInactive = mkColorSet color0 color12;
          unfocused = mkColorSet color0 color12;
        };
        bars = [{
          position = "top";
          command = "${pkgs.sway}/bin/swaybar";
          # command = "${pkgs.waybar}/bin/waybar";
          colors = {
            background = "#${background}";
            statusline = "#${color12}";
            separator = "#${color11}";
            focusedWorkspace = mkBarColorSet color9 color0;
            inactiveWorkspace = mkBarColorSet color0 color9;
          };
          trayOutput = "*";
        }];
      };
    };
  };
}
