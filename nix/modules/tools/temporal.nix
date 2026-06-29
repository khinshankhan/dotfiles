{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.temporal;
in {
  options.modules.tools.temporal = {
    enable = lib.mkEnableOption "temporal cli";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.temporal-cli ];
  };
}
