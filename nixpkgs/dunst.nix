{ pkgs ? import <nixpkgs> }: {
  enable = true;
  settings = {
    global = {
      font = "Fira Code 8";
      markup = true;
      plain_text = false;
      format = "<b>%s</b>\n%b";
      alignment = "left";
      bounce_freq = 0;
      show_age_threshold = 60;
      word_wrap = true;
      ignore_newline = false;
      stack_duplicates = true;
      geometry = "300x50-15+49";
      shrink = false;
      transparency = 0;
      idle_threshold = 0;
      monitor = 0;
      follow = "none";
      history_length = 15;
      sticky_history = true;
      show_indicators = true;
      line_height = 3;
      separator_height = 2;
      padding = 6;
      horizontal_padding = 6;
      separator_color = "frame";
      startup_notification = false;
      dmenu = "{pkgs.dmenu}/bin/dmenu -p dunst";
      browser = "{pkgs.firefox}/bin/firefox -new-tab";
      icon_position = false;
      max_icon_size = 80;
    };
    urgency_low = {
      frame_color = "#3B7C87";
      foreground = "#3B7C87";
      background = "#191311";
      timeout = 4;
    };
    urgency_normal = {
      frame_color = "#5B8234";
      foreground = "#5B8234";
      background = "#191311";
      timeout = 6;
    };
    urgency_critical = {
      frame_color = "#B7472A";
      foreground = "#B7472A";
      background = "#191311";
      timeout = 8;
    };
  };
}
