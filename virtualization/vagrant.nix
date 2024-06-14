{ config, lib, pkgs, ... }:

{
  virtualisation.virtualbox.host = {
    enable = true;
    enableExtensionPack = false;
  };

  # https://nixos.wiki/wiki/Vagrant

  ## Minimal configuration for NFS support with Vagrant.
  # services.nfs.server = {
  #   enable = true;
  #
  #   # For nfsv3
  #   lockdPort = 4001;
  #   mountdPort = 4002;
  #   statdPort = 4000;
  #
  #   exports = ''
  #   '';
  # };

  ## Add firewall exception for VirtualBox provider 
  # networking.firewall.extraCommands = ''
  #   ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
  # '';

  ## Add firewall exception for libvirt provider when using NFSv4 
  # networking.firewall.interfaces."virbr1" = {
  #   allowedTCPPorts = [ 2049 4001 4002 4003 ];
  #   # Current NixOS kernel options disable nfsd udp
  #   allowedUDPPorts = [ ];
  # };
}
