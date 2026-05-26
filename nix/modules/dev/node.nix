{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.node;
in {
  options.modules.dev.node = {
    enable = lib.mkEnableOption "node development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      pnpm
    ];
  };
}
