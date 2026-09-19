# gnupg --- signs, encrypts, and asks for the passphrase again
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.gnupg;
in {
  options.modules.tools.gnupg = {
    enable = lib.mkEnableOption "gnupg";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.gnupg ];
  };
}
