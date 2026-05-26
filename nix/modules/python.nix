{ lib, config, pkgs, ... }:

let
  cfg = config.modules.python;
in {
  options.modules.python = {
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
