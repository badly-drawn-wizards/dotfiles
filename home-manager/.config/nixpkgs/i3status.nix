{
  enable = true;
  config = {
    modules = {
      "disk /" = {
        position = 1;
        settings = { format = "%avail"; };
      };

      load = {
        position = 2;
        settings = { format = "%1min"; };
      };

      memory = {
        position = 3;
        settings = {
          format = "%used | %available";
          threshold_degraded = "1G";
          format_degraded = "MEMORY < %available";
        };
      };

      "tztime local" = {
        position = 4;
        settings = { format = "%Y-%m-%d %H:%M:%S"; };
      };
    };
  };
}
