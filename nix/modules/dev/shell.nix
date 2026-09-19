# shell --- she sells {ba,z,fi}sh shells on the C xor
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.shell;
in {
  options.modules.dev.shell = {
    enable = lib.mkEnableOption "shell development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      bash              # macos ships 3.2 from 2007 out of spite; this one is from this decade
      shellcheck        # tells you why the quotes matter, every single time
    ];
  };
}
