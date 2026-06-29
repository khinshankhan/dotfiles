{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.frp;
in {
  options.modules.tools.frp = {
    enable = lib.mkEnableOption "frp (fast reverse proxy: frpc + frps)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.frp ];
  };
}
