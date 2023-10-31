{ config, lib, pkgs, ... }:

{
  # virtualisation.oci-containers = {
  #   backend = "podman";
  #   containers.homeassistant = {
  #     volumes = [ "home-assistant:/config" ];
  #     environment.TZ = "Africa/Johannesburg";
  #     image = "ghcr.io/home-assistant/home-assistant:stable"; # Warning: if the tag does not change, the image will not be updated
  #     extraOptions = [
  #       "--network=host"
  #     ];
  #   };
  # };
  # networking.firewall.allowedTCPPorts = [ 8123 ];
}
