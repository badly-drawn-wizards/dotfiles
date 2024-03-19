{ lib, stdenvNoCC, fetchurl, androidenv, makeWrapper }:

let
 emulator = androidenv.emulateApp {
    name = "webnovel-emulate";
    app = fetchurl {
      url = "https://files.kingmodapk.net/apps/webnovel-mod-apk-v3.6-kingmodapk.net.apk";
      sha256 = "sha256-EDgcKJh909dmb99blDQRR6ZIUmwpIygmQRM/nGd0tJY=";
    };
    configOptions = {
      "hw.gpu.enabled" = "yes";
    };
    platformVersion = "30";
    package = "com.qidian.Int.reader";
    activity = "com.qidian.Int.reader.MainApplication";

    abiVersion = "x86";
    systemImageType = "google_apis_playstore";
  };
in stdenvNoCC.mkDerivation {
    # name = "webnovel-emulate";
    # nativeBuild
}
