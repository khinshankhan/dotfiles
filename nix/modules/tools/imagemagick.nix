# imagemagick --- pasted from a 2009 forum post, still works
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.imagemagick;
in {
  options.modules.tools.imagemagick = {
    enable = lib.mkEnableOption "imagemagick image processing";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.imagemagick ];
  };
}
