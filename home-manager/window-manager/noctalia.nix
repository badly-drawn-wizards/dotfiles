{ pkgs, config, ... }:

{
  programs.noctalia-shell = {
    enable = true;
    package = pkgs.noctalia-shell.override { calendarSupport = true; };
    settings = {
      appLauncher = {
        terminalCommand = "kitty -e";
      };
      bar.widgets = {
        left = [
          { id = "Launcher"; }
          {
            id = "Clock";
            formatHorizontal = "yyyy-MM-dd HH:mm";
          }
          { id = "SystemMonitor"; }
          { id = "ActiveWindow"; }
          { id = "MediaMini"; }
        ];
        center = [
          {
            id = "Workspace";
            characterCount = 10;
            labelMode = "name";
          }
        ];
        right = [
          {
            id = "CustomButton";
            icon = "rotate";
            leftClickExec = "iio-rotate";
          }
          { id = "Tray"; }
          { id = "NotificationHistory"; }
          { id = "Battery"; }
          { id = "Volume"; }
          { id = "Brightness"; }
          { id = "ControlCenter"; }
        ];
      };
      calendar.cards = [
        { id = "calendar-header-card"; enabled = true; }
        { id = "calendar-month-card"; enabled = true; }
        { id = "weather-card"; enabled = false; }
      ];
      colorSchemes.predefinedScheme = "Dracula";
      controlCenter.cards = [
        { id = "profile-card"; enabled = true; }
        { id = "shortcuts-card"; enabled = true; }
        { id = "audio-card"; enabled = true; }
        { id = "brightness-card"; enabled = true; }
        { id = "weather-card"; enabled = false; }
        { id = "media-sysmon-card"; enabled = true; }
      ];
      general = {
        compactLockScreen = true;
        clockStyle = "digital";
        showSessionButtonsOnLockScreen = false;
        lockScreenAnimations = true;
      };
      wallpaper.directory = "${config.home.homeDirectory}/backgrounds";
    };
  };

  windowManager.startupPrograms = [
    "${config.programs.noctalia-shell.package}/bin/noctalia-shell"
  ];
}
