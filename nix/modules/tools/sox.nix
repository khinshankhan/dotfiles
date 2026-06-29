{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.sox;
in {
  options.modules.tools.sox = {
    enable = lib.mkEnableOption "sox sound processor";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.sox ];
  };
}
