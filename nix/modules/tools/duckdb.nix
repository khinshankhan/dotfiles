{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.duckdb;
in {
  options.modules.tools.duckdb = {
    enable = lib.mkEnableOption "duckdb";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.duckdb ];
  };
}
