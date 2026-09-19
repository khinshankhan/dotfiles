# lua --- one-based indices? one-based indices
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.lua;
in {
  options.modules.dev.lua = {
    enable = lib.mkEnableOption "lua development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      lua               # one-based indices? one-based indices
      stylua            # formats lua, arguments not accepted
    ];
  };
}
