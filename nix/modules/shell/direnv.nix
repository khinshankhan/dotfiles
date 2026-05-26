{ lib, config, pkgs, ... }:

let
  cfg = config.modules.shell.direnv;
in {
  options.modules.shell.direnv = {
    enable = lib.mkEnableOption "direnv";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.direnv ];
  };
}
