{ lib, config, pkgs, ... }:

let
  cfg = config.modules.go;
in {
  options.modules.go = {
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
