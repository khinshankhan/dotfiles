{ lib, config, pkgs, ... }:

let
  cfg = config.modules.fonts.symbola;
in {
  options.modules.fonts.symbola = {
    enable = lib.mkEnableOption "symbola unicode font";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.symbola ];
  };
}
