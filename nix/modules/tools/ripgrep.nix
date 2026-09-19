# ripgrep --- grep, but it read the .gitignore first
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.ripgrep;
in {
  options.modules.tools.ripgrep = {
    enable = lib.mkEnableOption "ripgrep";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.ripgrep ];
  };
}
