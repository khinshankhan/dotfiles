# direnv --- cd in, come out with a different environment
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
