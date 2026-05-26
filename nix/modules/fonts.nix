{ lib, config, pkgs, ... }:

let
  cfg = config.modules.fonts;
in {
  options.modules.fonts = {
    enable = lib.mkEnableOption "font packages";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nerd-fonts.hack
      source-code-pro
      symbola
    ];
  };
}
