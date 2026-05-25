{ lib, config, pkgs, ... }:

let
  cfg = config.modules.zig;
in {
  options.modules.zig = {
    enable = lib.mkEnableOption "zig development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      zig
      zls
    ];
  };
}
