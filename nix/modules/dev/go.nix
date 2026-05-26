{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.go;
in {
  options.modules.dev.go = {
    enable = lib.mkEnableOption "go development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      delve
      go
      golangci-lint
      gopls
      gotools
    ];
  };
}
