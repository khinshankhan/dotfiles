{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.glow;
in {
  options.modules.tools.glow = {
    enable = lib.mkEnableOption "glow markdown renderer";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.glow ];
  };
}
