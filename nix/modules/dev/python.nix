# python --- beautiful is better than ugly
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.python;
in {
  options.modules.dev.python = {
    enable = lib.mkEnableOption "python development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      python3           # beautiful is better than ugly
      ruff              # flake8, black, and isort, in rust, before you blink
      pyright           # types for a language that shrugged at them
    ];
  };
}
