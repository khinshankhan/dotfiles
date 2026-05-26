{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.delta;
in {
  options.modules.tools.delta = {
    enable = lib.mkEnableOption "delta diff viewer";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.delta ];
  };
}
