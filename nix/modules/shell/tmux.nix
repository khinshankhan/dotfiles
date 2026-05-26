{ lib, config, pkgs, ... }:

let
  cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = {
    enable = lib.mkEnableOption "tmux";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.tmux ];
  };
}
