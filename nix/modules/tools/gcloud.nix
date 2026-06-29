{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.gcloud;
in {
  options.modules.tools.gcloud = {
    enable = lib.mkEnableOption "google cloud sdk (gcloud, gsutil, bq)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.google-cloud-sdk ];
  };
}
