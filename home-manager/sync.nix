{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ maestral-gui ];

  # millionants is the hub. ~/Dropbox goes there one way: Maestral stays the
  # only writer here, and the hub keeps a read-only copy. ~/Dropbox/.mignore
  # holds .stfolder/ so Maestral leaves Syncthing's marker alone.
  services.syncthing = {
    enable = true;
    settings = {
      options = {
        urAccepted = -1;
        relaysEnabled = false;
        globalAnnounceEnabled = false;
      };
      devices.millionants = {
        id = "5UR646Z-5B5CUAY-EYJC24D-ZVZUD7U-ZY5CJHS-577HZHS-WX5LF7U-CTF6JQP";
        # Tailscale, so it connects away from home too.
        addresses = [ "tcp://100.117.118.54:22000" "quic://100.117.118.54:22000" "dynamic" ];
      };
      folders.dropbox = {
        label = "Dropbox";
        path = "${config.home.homeDirectory}/Dropbox";
        type = "sendonly";
        devices = [ "millionants" ];
      };
    };
  };
}
