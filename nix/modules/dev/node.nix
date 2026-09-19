# node --- the runtime stays with nvm; nix only gets the accessories
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.node;
in {
  options.modules.dev.node = {
    enable = lib.mkEnableOption "node development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      pnpm              # npm, but node_modules is hardlinks and the disk thanks you
      vscode-js-debug   # vscode's debugger, liberated for dap
    ];
  };
}
