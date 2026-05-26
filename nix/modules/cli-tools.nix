{ lib, config, pkgs, ... }:

let
  cfg = config.modules.cli-tools;
in {
  options.modules.cli-tools = {
    enable = lib.mkEnableOption "common CLI tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      aspell
      coreutils
      delta
      direnv
      fd
      findutils
      gh
      git-lfs
      gnupg
      ripgrep
      shellcheck
      starship
      stylua
      tmux
      tree
    ];
  };
}
