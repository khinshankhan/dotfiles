{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.python;
in {
  options.modules.dev.python = {
    enable = lib.mkEnableOption "python development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      python3
      ruff
      pyright
    ];
  };
}
