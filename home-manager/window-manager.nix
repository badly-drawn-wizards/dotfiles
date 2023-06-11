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
    touchscreen = "1267:10741:ELAN2514:00_04F3:29F5";
    touchpad = "1739:52560:SYNA3297:00_06CB:CD50_Touchpad";
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
  mod = config.wayland.windowManager.sway.config.modifier;
  terminal = "${pkgs.kitty}/bin/kitty";

  rofi-modi-cmd = config.programs.rofi.modi-cmd;

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

    programs.waybar = {
      enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          modules-left =
            ["sway/workspaces" "sway/mode"];
          modules-center = ["sway/window"];
          modules-right =
            ["battery" "cpu" "memory" "clock" "tray"];
        };
      };
      style = ./waybar.css;
    };

    home.file.".config/waybar/dracula" = {
      source = let
        dracula-waybar-src = pkgs.fetchFromGitHub {
          owner = "dracula";
          repo = "waybar";
          rev = "3dd04357db89c0bb7f9635848c50cba827246fe3";
          sha256 = "sha256-ajHz3daePqkSbjVbYDAucoXEF/bj4A9qBrFBIw278Bg=";
        };
        dracula-waybar = builtins.path {
          path = "${dracula-waybar-src}/waybar";
          filter = path: type: lib.hasSuffix ".css" path;
        };
        in dracula-waybar;
    };

    wayland.windowManager.hyprland = {
      enable = true;
      xwayland = {
        enable = true;
        hidpi = true;
      };
    };

    wayland.windowManager.sway = {
      enable = true;

      extraSessionCommands = ''
        . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
      '';

      config = {
        inherit terminal;
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
          ${io.keyboard} = {
            xkb_layout = "us";
            xkb_options = "caps:swapescape";
            # repeat_rate = "0";
          };
          ${io.touchscreen} = {
            map_to_output = io.monitor;
          };
          ${io.touchpad} = {
            natural_scroll = "enabled";
          };
          ${io.stylus} = {
            map_to_output = io.monitor;
          };
        };
        output = {
          ${io.monitor} = {
            scale = "2";
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

            "${mod}+Shift+r" = "reload";
            "${mod}+i" = "exec em";
            "${mod}+m" = "exec mpv-paste";

            "${mod}+o" = "exec ${rofi-modi-cmd "workspace"}";
            "${mod}+Shift+o" = "exec ${rofi-modi-cmd "move"}";
            # "${mod}+p" = "exec ${rofi-modi-cmd "window"}";

            "${mod}+t" = "exec ${pkgs.gnome.nautilus}/bin/nautilus";
            "${mod}+Print" = ''exec ${pkgs.grim}/bin/grim -t png -g "$(${pkgs.slurp}/bin/slurp)" ${config.home.homeDirectory}/screenshots/$(date +%Y-%m-%d_%H-%m-%s).png'';
            "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +10%";
            "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%-";
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
        bars = [{
          position = "top";
          command = "${config.programs.waybar.package}/bin/waybar";
          # statusCommand = "${pkgs.python39Packages.py3status}/bin/py3status";
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
