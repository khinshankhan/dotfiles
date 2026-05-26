{ lib, config, pkgs, ... }:

let
  cfg = config.modules.system.findutils;
in {
  options.modules.system.findutils = {
    enable = lib.mkEnableOption "gnu findutils";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.findutils ];
  };
}
