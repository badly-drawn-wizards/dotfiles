{ config, lib, pkgs, ... }:

let
  # Fetch piper model files
  piperModel = pkgs.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/ryan/high/en_US-ryan-high.onnx?download=true";
    hash = "sha256-s5kNdgbhg+yNv7pwpGBwdPFi3hoMQS4BgNH/YLsVTso=";
  };

  piperModelJson = pkgs.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/ryan/high/en_US-ryan-high.onnx.json?download=true";
    hash = lib.fakeHash;
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

    AddVoice "en_US" "MALE1" "Piper"
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
