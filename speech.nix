{ config, lib, pkgs, ... }:

let
  # Define voices to include
  voices = [
    {
      lang = "en";
      region = "GB";
      name = "alan";
      quality = "medium";
      onnxHash = "sha256-CjCWaJMiBedigB8e/Cc2zUsBIDKWIq32K+CeVjOdMzA=";
      jsonHash = "sha256-wPDRJOWJXADnwDs13MgofzGaaZijZbGC3rXI51LujB4=";
    }
    {
      lang = "en";
      region = "US";
      name = "ryan";
      quality = "high";
      onnxHash = "sha256-s5kNdgbhg+yNv7pwpGBwdPFi3hoMQS4BgNH/YLsVTso=";
      jsonHash = "sha256-xtO5jwgxXLS+vw1J1Q/E/0kbUDxkuUDNPVyihUO0gBE=";
    }
  ];

  # Function to get filename from voice attrset
  voiceFilename = voice: "${voice.lang}_${voice.region}-${voice.name}-${voice.quality}";

  # Function to get URL for a voice file
  voiceUrl = voice: suffix:
    let
      locale = "${voice.lang}_${voice.region}";
      base = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0";
    in "${base}/${voice.lang}/${locale}/${voice.name}/${voice.quality}/${voiceFilename voice}.onnx${suffix}?download=true";

  # Fetch all voice files
  fetchVoiceFiles = voices:
    lib.listToAttrs (lib.flatten (map (voice:
      let filename = voiceFilename voice;
      in [
        {
          name = "${filename}-onnx";
          value = pkgs.fetchurl {
            url = voiceUrl voice "";
            hash = voice.onnxHash;
          };
        }
        {
          name = "${filename}-json";
          value = pkgs.fetchurl {
            url = voiceUrl voice ".json";
            hash = voice.jsonHash;
          };
        }
      ]) voices));

  voiceFiles = fetchVoiceFiles voices;

  # Combine all models into a single derivation
  piperModel = pkgs.runCommand "piper-voices" voiceFiles ''
    mkdir -p $out
    ${lib.concatMapStringsSep "\n" (voice:
      let filename = voiceFilename voice;
      in ''
        cp ${voiceFiles."${filename}-onnx"} $out/${filename}.onnx
        cp ${voiceFiles."${filename}-json"} $out/${filename}.onnx.json
      '') voices}
  '';

  # Generate voice definitions
  generateVoiceDefinitions = voices:
    lib.concatMapStringsSep "\n" (voice:
      let
        locale = "${voice.lang}_${voice.region}";
        filename = voiceFilename voice;
      in ''AddVoice "${locale}" "MALE1" "${filename}"''
    ) voices;

  # Piper synthesis script
  piperSynthScript = pkgs.writeShellScriptBin "piper-synth" ''
    DATA="$1"
    RATE="$2"
    PITCH="$3"
    PLAY_COMMAND="$4"
    VOICE="$5"

    # Default to first voice if no_voice is given
    if [ "$VOICE" = "no_voice" ]; then
      VOICE="${voiceFilename (builtins.head voices)}"
    fi

    export PATH=${lib.makeBinPath [ pkgs.sox pkgs.pulseaudio pkgs.pipewire pkgs.piper-tts ]}:$PATH

    echo "$DATA" | piper --model ${piperModel}/$VOICE.onnx -s 0 --output_raw | \
    sox -r 22050 -c 1 -b 16 -e signed-integer -t raw - -t wav - tempo $RATE pitch $PITCH norm | \
    $PLAY_COMMAND
  '';

in
{
  # Enable and configure speechd
  services.speechd = {
    enable = true;
    modules.piper = ''
      Debug 1
      GenericExecuteSynth "${piperSynthScript}/bin/piper-synth '$DATA' '$RATE' '$PITCH' '$PLAY_COMMAND' '$VOICE'"

      GenericRateAdd 1
      GenericPitchAdd 1
      GenericVolumeAdd 1
      GenericRateMultiply 1
      GenericPitchMultiply 1000

      ${generateVoiceDefinitions voices}
    '';
    config = ''
      LogDir "/var/log/speech-dispatcher"
      AddModule "piper" "sd_generic" "piper.conf"
      DefaultModule "piper"
    '';
  };

  environment.systemPackages = [
    piperSynthScript
  ];

}
