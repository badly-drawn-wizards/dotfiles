{ config, lib, pkgs, ... }:

{
  hardware = {
    sane = {
      enable = true;
    };
  };

  services = {
    printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        gutenprintBin
        brlaser
        brgenml1lpr
        brgenml1cupswrapper
      ];
    };

    avahi = {
      enable = true;
      nssmdns4 = false; # See note below
      openFirewall = true;
    };
  };

  # Attempt to only use mdns on ipv4
  system.nssModules = pkgs.lib.optional (!config.services.avahi.nssmdns4) pkgs.nssmdns;
  system.nssDatabases.hosts = with pkgs.lib; optionals (!config.services.avahi.nssmdns4) (mkMerge [
    (mkBefore [ "mdns4_minimal [NOTFOUND=return]" ]) # before resolve
    (mkAfter [ "mdns4" ]) # after dns
  ]);

}
