{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.doppler;
in {
  options.modules.tools.doppler = {
    enable = lib.mkEnableOption "doppler secrets cli";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.doppler ];
  };
}
