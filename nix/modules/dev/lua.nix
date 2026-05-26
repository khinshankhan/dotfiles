{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.lua;
in {
  options.modules.dev.lua = {
    enable = lib.mkEnableOption "lua development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      lua
      stylua
    ];
  };
}
