{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.zig;
in {
  options.modules.dev.zig = {
    enable = lib.mkEnableOption "zig development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      zig
      zls
    ];
  };
}
