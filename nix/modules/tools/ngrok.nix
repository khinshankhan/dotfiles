{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.ngrok;
in {
  options.modules.tools.ngrok = {
    enable = lib.mkEnableOption "ngrok tunneling cli";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.ngrok ];
  };
}
