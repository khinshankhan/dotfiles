{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.gh;
in {
  options.modules.tools.gh = {
    enable = lib.mkEnableOption "github cli";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.gh ];
  };
}
