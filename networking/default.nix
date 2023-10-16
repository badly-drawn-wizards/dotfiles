{ config, lib, pkgs, ... }:

{
  networking = {
    hostName = "noobnoob";
    networkmanager.enable = true;
    hosts =
    {
      "127.0.0.1" = [
        # "reddit.com"
        # "www.reddit.com"
        # "news.ycombinator.com"
        # "youtube.com"
        # "www.youtube.com"
      ];
    };

    firewall = {
      enable = true;
      checkReversePath = false;
    };
  };
}
