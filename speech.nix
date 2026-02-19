{ config, lib, pkgs, ... }:

let
  # Fetch piper model
  piperModel = pkgs.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_GB/alan/medium/en_GB-alan-medium.onnx?download=true";
    hash = "sha256-CjCWaJMiBedigB8e/Cc2zUsBIDKWIq32K+CeVjOdMzA=";
  };

  # Piper module configuration file
  piperConf = pkgs.writeText "piper.conf" ''
    GenericExecuteSynth "export PATH=${lib.makeBinPath [ pkgs.sox pkgs.pulseaudio pkgs.piper-tts ]}:$PATH; \
    export PLAY_COMMAND=paplay; \
    echo '$DATA' | piper --model ${piperModel} -s 0 --output_raw | \
    sox -r 22050 -c 1 -b 16 -e signed-integer -t raw - -t wav - tempo $RATE pitch $PITCH norm | \
    $PLAY_COMMAND;"

    GenericRateAdd 1
    GenericPitchAdd 1
    GenericVolumeAdd 1
    GenericRateMultiply 1
    GenericPitchMultiply 1000

    AddVoice "en_GB" "MALE1" "Piper"
  '';
in
{
  # Enable and configure speechd
  services.speechd = {
    enable = true;
    config = ''
      AddModule "piper" "sd_generic" "${piperConf}"
      DefaultModule "piper"
    '';
  };
}
