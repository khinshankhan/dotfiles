{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.mise;
in {
  options.modules.dev.mise = {
    enable = lib.mkEnableOption "mise version manager";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.mise ];
  };
}
