{ lib, config, pkgs, ... }:

let
  cfg = config.modules.dev.go;
in {
  options.modules.dev.go = {
    enable = lib.mkEnableOption "go development tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      delve
      go
      golangci-lint
      gopls
      # gotools ships a generic `play` (the Go Playground server) that collides
      # with sox's `play` in the nix profile. Rename it to `go-play` so both coexist.
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
