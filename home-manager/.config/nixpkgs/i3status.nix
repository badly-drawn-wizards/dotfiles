{
  enable = true;
  enableDefault = false;
  modules = {
    load = {
      position = 1;
      settings = { format = "%1min CPU"; };
    };

    memory = {
      position = 2;
      settings = {
        format = "%used + %available";
      };
    };

    "disk /" = {
      position = 3;
      settings = { format = "%avail / %total"; };
    };

    "tztime local" = {
      position = 4;
      settings = { format = "%Y-%m-%d %H:%M:%S"; };
    };
  };
}
