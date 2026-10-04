# jq --- sed for json, if sed could count brackets
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.jq;
in {
  options.modules.tools.jq = {
    enable = lib.mkEnableOption "jq json processor";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.jq ];
  };
}
