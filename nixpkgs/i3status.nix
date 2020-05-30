let
  cpu = "";
  memory = "";
  storage = "";
  clock = "";
  calendar = "";
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
      settings = { format = "${cpu} %1min "; };
    };

    memory = {
      position = 2;
      settings = {
        format = " ${memory} %used + %available ";
      };
    };

    "disk /" = {
      position = 3;
      settings = { format = " ${storage} %avail / %total "; };
    };

    "tztime local" = {
      position = 4;
      settings = { format = " ${clock} %H:%M:%S ${calendar} %d/%m"; };
    };
  };
}
