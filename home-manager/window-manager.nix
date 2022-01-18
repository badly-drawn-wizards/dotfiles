{ pkgs
, lib
, config
, ...
}:
with builtins;
with config.theme;
let
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
      { app_id = "^calibre-gui$"; }
      { class = "^MuPdf$"; }
      { class = "^okular$"; }
    ];
    " chat" = [
      { class = "^discord$"; }
      { class = "^Riot$"; }
      { class = "^Slack$"; }
      { class = "^Thunderbird$"; }
    ];
    " write" = [
      { class = "^Write$"; }
      { app_id = "^com.github.xournalpp.xournalpp"; }
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

    # window-manager.startupPrograms = with pkgs; [
      # "${xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr"
      # "${xdg-desktop-portal}/libexec/xdg-desktop-portal -r"
    # ];

    home.sessionVariables = {
      XDG_SESSION_TYPE="wayland";
      XDG_CURRENT_DESKTOP="sway";

      # Get sway to play nicely with IntelliJ
      _JAVA_AWT_WM_NONREPARENTING=1;
    };

    # home.packages = with pkgs; [
    #   xdg-desktop-portal
    #   xdg-desktop-portal-wlr
    # ];

    programs.rofi = {
      enable = true;
      font = "Fira Code 8";
      terminal = "${pkgs.rxvt-unicode}/bin/urxvt";
      extraConfig = {
        modi = "run,window,workspace:${rofi-workspace},move:${rofi-move}";
        kb-row-tab = "";
        kb-remove-to-eol = "";
        kb-accept-entry = "Return";
        kb-mode-next = "Tab";
        kb-mode-previous = "Shift+Tab";
        kb-row-up = "Control+k";
        kb-row-down = "Control+j";
      };
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
          "1:1:AT_Translated_Set_2_keyboard" = {
            xkb_layout = "us";
            xkb_options = "caps:swapescape";
          };
          "ELAN2514:00 04F3:29F5" = {
            map_to_output = "eDP-1";
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
            "${mod}+m" = "exec mpv-paste";
            "XF86PowerOff" = "exec toggle-rot8";
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
