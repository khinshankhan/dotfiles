{ lib, config, pkgs, ... }:

let
  cfg = config.modules.shell.starship;
in {
  options.modules.shell.starship = {
    enable = lib.mkEnableOption "starship prompt";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.starship ];
  };
}
