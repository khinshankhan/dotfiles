{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.shell;
in {
  options.modules.dev.shell = {
    enable = lib.mkEnableOption "shell development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      bash
      shellcheck
    ];
  };
}
