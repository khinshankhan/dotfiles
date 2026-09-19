# herdr --- a shepherd for agents that wander off
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.herdr;
in {
  options.modules.tools.herdr = {
    enable = lib.mkEnableOption "herdr agent multiplexer";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.herdr ];
  };
}
