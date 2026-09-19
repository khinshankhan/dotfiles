# awscli --- three-letter services, subcommands all the way down
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.awscli;
in {
  options.modules.tools.awscli = {
    enable = lib.mkEnableOption "aws cli";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.awscli2 ];
  };
}
