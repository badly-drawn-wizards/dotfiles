{ config, lib, pkgs, ... }:

let
  mod = config.wayland.windowManager.sway.config.modifier;
  inherit (config) theme;
in
{
  services.swaync = {
    enable = true;
    settings = {
      positionX = "right";
      positionY = "top";
      layer = "overlay";
      control-center-layer = "top";
      layer-shell = true;
      cssPriority = "application";
      control-center-margin-top = 0;
      control-center-margin-bottom = 0;
      control-center-margin-right = 0;
      control-center-margin-left = 0;
      notification-2fa-action = true;
      notification-inline-replies = false;
      notification-icon-size = 64;
      notification-body-image-height = 100;
      notification-body-image-width = 200;
      timeout = 10;
      timeout-low = 5;
      timeout-critical = 0;
      fit-to-screen = true;
      control-center-width = 500;
      control-center-height = 600;
      notification-window-width = 500;
      keyboard-shortcuts = true;
      image-visibility = "when-available";
      transition-time = 200;
      hide-on-clear = false;
      hide-on-action = true;
      script-fail-notify = true;

      widgets = [
        "inhibitors"
        "title"
        "dnd"
        "mpris"
        "notifications"
      ];

      widget-config = {
        inhibitors = {
          text = "Inhibitors";
          button-text = "Clear All";
          clear-all-button = true;
        };
        title = {
          text = "Notifications";
          clear-all-button = true;
          button-text = "Clear All";
        };
        dnd = {
          text = "Do Not Disturb";
        };
        mpris = {
          image-size = 96;
          image-radius = 12;
        };
      };
    };

    style = ''
      * {
        all: unset;
        font-size: 14px;
        font-family: "${config.font}", monospace;
      }

      .notification-row {
        outline: none;
        margin: 0;
        padding: 0px;
      }

      .notification {
        background: #${theme.background};
        border-radius: 12px;
        margin: 6px 12px;
        box-shadow: 0 0 8px 0 rgba(0, 0, 0, 0.8);
        padding: 0;
      }

      .notification-content {
        background: transparent;
        padding: 6px;
        border-radius: 12px;
      }

      .close-button {
        background: #${theme.color1};
        color: #${theme.foreground};
        text-shadow: none;
        padding: 0;
        border-radius: 12px;
        margin-top: 6px;
        margin-right: 6px;
      }

      .close-button:hover {
        box-shadow: none;
        background: #${theme.color9};
        color: #${theme.foreground};
      }

      .notification-default-action,
      .notification-action {
        padding: 4px;
        margin: 0;
        box-shadow: none;
        background: #${theme.selection_background};
        border: 1px solid #${theme.color12};
        color: #${theme.foreground};
        border-radius: 12px;
      }

      .notification-default-action:hover,
      .notification-action:hover {
        -gtk-icon-effect: none;
        background: #${theme.color12};
      }

      .notification-default-action {
        border-radius: 12px;
      }

      .summary {
        font-size: 16px;
        font-weight: bold;
        background: transparent;
        color: #${theme.color5};
        text-shadow: none;
      }

      .time {
        font-size: 12px;
        font-weight: bold;
        background: transparent;
        color: #${theme.foreground};
        text-shadow: none;
        margin-right: 18px;
      }

      .body {
        font-size: 14px;
        font-weight: normal;
        background: transparent;
        color: #${theme.foreground};
        text-shadow: none;
      }

      .control-center {
        background: #${theme.background};
        border: 2px solid #${theme.color5};
        border-radius: 12px;
      }

      .control-center-list {
        background: transparent;
      }

      .control-center-list-placeholder {
        opacity: 0.5;
      }

      .floating-notifications {
        background: transparent;
      }

      .blank-window {
        background: alpha(black, 0.1);
      }

      .widget-title {
        color: #${theme.color5};
        background: #${theme.selection_background};
        padding: 8px;
        margin: 12px 12px 6px 12px;
        font-size: 18px;
        border-radius: 12px;
      }

      .widget-title > button {
        font-size: 14px;
        color: #${theme.foreground};
        text-shadow: none;
        background: #${theme.color1};
        border: 1px solid #${theme.color1};
        box-shadow: none;
        border-radius: 12px;
      }

      .widget-title > button:hover {
        background: #${theme.color9};
        border: 1px solid #${theme.color9};
      }

      .widget-dnd {
        background: #${theme.selection_background};
        padding: 8px;
        margin: 6px 12px;
        border-radius: 12px;
        font-size: 18px;
        color: #${theme.foreground};
      }

      .widget-dnd > switch {
        font-size: 18px;
        border-radius: 12px;
        background: #${theme.color12};
        border: 1px solid #${theme.color5};
      }

      .widget-dnd > switch:checked {
        background: #${theme.color2};
        border: 1px solid #${theme.color2};
      }

      .widget-dnd > switch slider {
        background: #${theme.foreground};
        border-radius: 12px;
      }

      .widget-mpris {
        background: #${theme.selection_background};
        padding: 8px;
        margin: 6px 12px;
        border-radius: 12px;
      }

      .widget-mpris-player {
        padding: 8px;
        margin: 8px;
      }

      .widget-mpris-title {
        font-weight: bold;
        font-size: 16px;
        color: #${theme.color5};
      }

      .widget-mpris-subtitle {
        font-size: 14px;
        color: #${theme.color6};
      }

      .widget-inhibitors {
        background: #${theme.selection_background};
        padding: 8px;
        margin: 6px 12px;
        border-radius: 12px;
        font-size: 18px;
        color: #${theme.foreground};
      }

      .widget-inhibitors > button {
        font-size: 14px;
        color: #${theme.foreground};
        text-shadow: none;
        background: #${theme.color1};
        border: 1px solid #${theme.color1};
        box-shadow: none;
        border-radius: 12px;
      }

      .widget-inhibitors > button:hover {
        background: #${theme.color9};
        border: 1px solid #${theme.color9};
      }
    '';
  };

  windowManager.extraBinds = {
    "${mod}+n" = "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
    "${mod}+Shift+n" = "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
  };
}
