# android --- a phone OS whose SDK is bigger than some phones
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.android;

  # The SDK lives in the read-only nix store, so the Android Gradle Plugin
  # can't download what's missing. Projects pin compileSdk and
  # buildToolsVersion to what's listed here; bump both together.
  sdk = (pkgs.androidenv.composeAndroidPackages {
    platformVersions = [ "35" ];
    buildToolsVersions = [ "35.0.0" ];
    includeEmulator = false;
    includeSystemImages = false;
    includeNDK = false;
    includeCmake = false;  # only for native code, which this SDK has none of
  }).androidsdk;
in {
  options.modules.dev.android = {
    enable = lib.mkEnableOption "android development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      jdk21             # gradle wants a real JDK, not the system's java 8
      kotlin-language-server # completion for the language that isn't java
      sdk               # platform, build-tools, platform-tools (adb)
    ];

    home.sessionVariables = {
      ANDROID_HOME = "${sdk}/libexec/android-sdk";
      JAVA_HOME = "${pkgs.jdk21}/lib/openjdk";
    };
  };
}
