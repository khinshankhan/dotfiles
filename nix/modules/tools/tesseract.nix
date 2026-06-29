{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.tesseract;
in {
  options.modules.tools.tesseract = {
    enable = lib.mkEnableOption "tesseract OCR (all languages)";
  };

  config = lib.mkIf cfg.enable {
    # all-languages build, matching brew's tesseract + tesseract-lang
    home.packages = [ (pkgs.tesseract.override { enableLanguages = null; }) ];
  };
}
