{ lib, config, pkgs, ... }:

let
  cfg = config.modules.node;
in {
  options.modules.node = {
    enable = lib.mkEnableOption "node development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      pnpm
    ];
  };
}
