{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.nix;
in {
  options.modules.dev.nix = {
    enable = lib.mkEnableOption "nix development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.nil ];
  };
}
