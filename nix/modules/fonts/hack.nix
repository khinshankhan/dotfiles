{ lib, config, pkgs, ... }:

let
  cfg = config.modules.fonts.hack;
in {
  options.modules.fonts.hack = {
    enable = lib.mkEnableOption "hack nerd font";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.nerd-fonts.hack ];
  };
}
