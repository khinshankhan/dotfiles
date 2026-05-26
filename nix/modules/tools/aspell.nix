{ lib, config, pkgs, ... }:

let
  cfg = config.modules.tools.aspell;
in {
  options.modules.tools.aspell = {
    enable = lib.mkEnableOption "aspell spell checker";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      (pkgs.aspellWithDicts (dicts: with dicts; [
        en
      ]))
    ];
  };
}
