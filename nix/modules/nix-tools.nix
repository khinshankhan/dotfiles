{ lib, config, pkgs, ... }:

let
  cfg = config.modules.nix-tools;
in {
  options.modules.nix-tools = {
    enable = lib.mkEnableOption "nix development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nil
    ];
  };
}
