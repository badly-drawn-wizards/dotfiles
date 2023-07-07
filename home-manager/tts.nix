{ config, lib, pkgs, ... }:

let
  speechdDir = ".config/speech-dispatcher";
  configFile = "${speechdDir}/speechd.conf";
  moduleDir = "${speechdDir}/modules";
  speechd = pkgs.speechd.override { withPulse = true; withAlsa = true; };
  ryan-model = pkgs.fetchzip {
    url = "https://github.com/rhasspy/piper/releases/download/v0.0.2/voice-en-us-ryan-high.tar.gz";
    stripRoot=false;
    sha256 = "sha256-BbAoryDFYD95DK7PactjeQkhLTbhEmnHfbZJRaZ2EBk=";
  };
  tts-packages = with pkgs; [mimic piper-tts espeak-ng coreutils sox];
in
{
    systemd.user.services.speech-dispatcher = {
      Unit = {
        Description = "speech-dispatcher";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        Environment = [
          "HOME=${config.home.homeDirectory}"
          "PATH=${lib.makeBinPath tts-packages}"
        ];
        Type="forking";
        ExecStart = "${speechd}/bin/speech-dispatcher -d -t0 -C ${config.home.homeDirectory}/${speechdDir}";
        ExecReload="kill -HUP $MAINPID";
        Restart = "on-failure";
      };
    };

    home.file = {
      ${configFile}.text = ''
        LogLevel 3
        LogDir /tmp
        # AddModule "piper" "sd_generic" "piper.conf"
        AddModule "mimic" "sd_generic" "mimic.conf"
        AddModule "espeak-ng" "sd_espeak-ng" "espeak-ng.conf"
        AddModule "dummy" "sd_dummy" ""

        DefaultVoiceType "MALE1"
        DefaultModule "mimic"
        DefaultLanguage "en"
        AudioOutputMethod "libao"
      '';

      ".local/share/piper/models/ryan".source = ryan-model;

      "${moduleDir}/piper.conf".text = ''
        GenericRateAdd 1.6
        GenericRateMultiply 0.01
        GenericExecuteSynth "printf %s \'$DATA\' | ${pkgs.piper-tts}/bin/piper -m ${ryan-model}/en-us-ryan-high.onnx -f - | ${pkgs.sox}/bin/play - tempo \"$RATE\""
        AddVoice "en" "MALE1" "piper"
      '';
      "${moduleDir}/mimic.conf".text = ''
        GenericRateAdd 1.6
        GenericRateMultiply 0.01
        GenericExecuteSynth "printf %s \'$DATA\' | ${pkgs.docker}/bin/docker exec -it run-venv mimic3 --remote --voice \'$VOICE\' --stdout | ${pkgs.sox}/bin/play - tempo \"$RATE\""
        AddVoice "en" "MALE1" "mimic"
      '';
    };

    home.packages = [
      speechd
    ] ++ tts-packages;
}
