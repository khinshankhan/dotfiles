{ lib, config, pkgs, ... }:

let
  cfg = config.modules.fonts.jetbrains-mono;
in {
  options.modules.fonts.jetbrains-mono = {
    enable = lib.mkEnableOption "jetbrains mono nerd font";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.nerd-fonts.jetbrains-mono ];
  };
}
