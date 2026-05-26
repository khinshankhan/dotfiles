{ lib, config, pkgs, ... }:

let
  cfg = config.modules.fonts.source-code-pro;
in {
  options.modules.fonts.source-code-pro = {
    enable = lib.mkEnableOption "source code pro font";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.source-code-pro ];
  };
}
