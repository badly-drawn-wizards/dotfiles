let
  cpu = "";
  memory = "";
  storage = "";
in
{
  enable = true;
  enableDefault = false;
  general = {
    separator = " / ";
  };
  modules = {
    load = {
      position = 1;
      settings = { format = "%1min ${cpu}"; };
    };

    memory = {
      position = 2;
      settings = {
        format = "%used ${memory} %available";
      };
    };

    "disk /" = {
      position = 3;
      settings = { format = "%avail ${storage} %total"; };
    };

    "tztime local" = {
      position = 4;
      settings = { format = "%m-%d %H:%M"; };
    };
  };
}
