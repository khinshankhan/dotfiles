{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.fd;
in {
  options.modules.tools.fd = {
    enable = lib.mkEnableOption "fd file finder";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.fd ];
  };
}
