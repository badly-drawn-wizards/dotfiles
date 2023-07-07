{ config, lib, pkgs, ... }:

let
  inherit (pkgs.dockerTools) pullImage buildImage;
  baseImage = pkgs.dockerTools.pullImage {
    imageName = "jtabet/mimic3";
    finalImageTag = "latest";
    imageDigest = "sha256:afa60f71ede9ed02d13dbf2870ad81b349049ac977655275f4565a85b961112f";
    sha256 = "sha256-RTeBlB67N9dF0E0E7JEE/lHNwUb5qHUBa0c8+v0VxLA=";
  };

  imageFile = buildImage {
    name = "mimic3-container";
    tag = "latest";
    fromImage = baseImage;
    copyToRoot = pkgs.buildEnv {
      name = "mimic3-additional";
      pathsToLink = [ "/" ];
      paths = [
        pkgs.alsa-utils
        pkgs.pipewire
        (pkgs.writeScriptBin "run-venv" ''
        #!/usr/bin/env bash
        source $HOME/app/.venv/bin/activate
        exec "$@"
        '')
      ];
    };
  };
in
{
  virtualisation.oci-containers.containers."mimic3" = {
    autoStart = true;
    image = "mimic3-container";
    inherit imageFile;
    ports = [
      "127.0.0.1:59125:59125"
    ];
    volumes = [
      "mimic3:/home/mimic3"
      "/run/user/1000/pipewire-0:/run/user/1000/pipewire-0"
    ];
    environment = {
     XDG_RUNTIME_DIR = "/run/user/1000";
    };
    user = "mimic3";
    entrypoint = "run-venv";
    cmd = ["mimic3-server" "--play-program" "pw-play --rate 22050 --channels 1 --format s16 -"];
  };
}
