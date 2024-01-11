{ lib, fetchurl, androidenv }:

androidenv.emulateApp {
  name = "Webnovel";
  app = fetchurl {
    url = "https://files.kingmodapk.net/apps/webnovel-mod-apk-v3.6-kingmodapk.net.apk";
    sha256 = "sha256-EDgcKJh909dmb99blDQRR6ZIUmwpIygmQRM/nGd0tJY=";
  };
  platformVersion = "30";
  package = "com.qidian.Int.reader";
  activity = "com.qidian.Int.reader.MainApplication";

  abiVersion = "x86";
  systemImageType = "google_apis_playstore";
}


