{ config, lib, pkgs, ... }:

let
  # Fetch piper model files
  piperModelOnnx = pkgs.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_GB/alan/medium/en_GB-alan-medium.onnx?download=true";
    hash = "sha256-CjCWaJMiBedigB8e/Cc2zUsBIDKWIq32K+CeVjOdMzA=";
  };

  piperModelJson = pkgs.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_GB/alan/medium/en_GB-alan-medium.onnx.json?download=true";
    hash = "sha256-wPDRJOWJXADnwDs13MgofzGaaZijZbGC3rXI51LujB4=";
  };

  # Combine model files into a single derivation
  piperModel = pkgs.runCommand "piper-alan-medium"
    {
      onnx = piperModelOnnx;
      json = piperModelJson;
    } ''
    mkdir -p $out
    cp $onnx $out/en_GB-alan-medium.onnx
    cp $json $out/en_GB-alan-medium.onnx.json
  '';

  # Piper synthesis script
  piperSynthScript = pkgs.writeShellScriptBin "piper-synth" ''
    DATA="$1"
    RATE="$2"
    PITCH="$3"
    PLAY_COMMAND="$4"

    export PATH=${lib.makeBinPath [ pkgs.sox pkgs.pulseaudio pkgs.piper-tts ]}:$PATH
    export PLAY_COMMAND=paplay

    echo "$DATA" | piper --model ${piperModel}/en_GB-alan-medium.onnx -s 0 --output_raw | \
    sox -r 22050 -c 1 -b 16 -e signed-integer -t raw - -t wav - tempo $RATE pitch $PITCH norm | \
    $PLAY_COMMAND
  '';

in
{
  # Enable and configure speechd
  services.speechd = {
    enable = true;
    modules.piper = ''
      GenericExecuteSynth "${piperSynthScript}/bin/piper-synth '$DATA' '$RATE' '$PITCH' '$PLAY_COMMAND'"

      GenericRateAdd 1
      GenericPitchAdd 1
      GenericVolumeAdd 1
      GenericRateMultiply 1
      GenericPitchMultiply 1000

      AddVoice "en_GB" "MALE1" "Piper"
    '';
    config = ''
      LogDir "/var/log/speech-dispatcher/"
      AddModule "piper" "sd_generic" "piper.conf"
      DefaultModule "piper"
    '';
  };

  environment.systemPackages = [
    piperSynthScript
  ];

}
