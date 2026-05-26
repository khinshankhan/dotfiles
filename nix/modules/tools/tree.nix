{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.tree;
in {
  options.modules.tools.tree = {
    enable = lib.mkEnableOption "tree directory listing";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.tree ];
  };
}
