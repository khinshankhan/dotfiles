# go --- the hipster dialect, plus the entourage it brought along
{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.go;
in {
  options.modules.dev.go = {
    enable = lib.mkEnableOption "go development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      delve             # a debugger named after digging, which is what you'll be doing
      go                # the hipster dialect
      golangci-lint     # forty linters in a trenchcoat
      gopls             # the language server, pronounced 'go please'
      # gotools: goimports and friends. it also ships a generic `play` (the Go
      # Playground server) that collides with sox's `play` in the nix profile.
      # Rename it to `go-play` so both coexist.
      (gotools.overrideAttrs (old: {
        postInstall = (old.postInstall or "") + ''
          if [ -e "$out/bin/play" ]; then
            mv "$out/bin/play" "$out/bin/go-play"
          fi
        '';
      }))
    ];
  };
}
