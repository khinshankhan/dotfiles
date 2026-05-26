{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.git-lfs;
in {
  options.modules.tools.git-lfs = {
    enable = lib.mkEnableOption "git large file storage";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.git-lfs ];
  };
}
