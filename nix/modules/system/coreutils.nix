{ lib, config, pkgs, ... }:

let
  cfg = config.modules.system.coreutils;
in {
  options.modules.system.coreutils = {
    enable = lib.mkEnableOption "gnu coreutils";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.coreutils ];
  };
}
