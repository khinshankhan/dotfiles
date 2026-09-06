{ lib, config, pkgs, ... }:

let
  cfg = config.modules.fonts.noto-color-emoji;
in {
  options.modules.fonts.noto-color-emoji = {
    enable = lib.mkEnableOption "noto color emoji font";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.noto-fonts-color-emoji ];
  };
}
