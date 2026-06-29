{
  system = "aarch64-darwin";

  module = { pkgs, ... }: {
    imports = [
      ../../modules/dev/go.nix
      ../../modules/dev/nix.nix
      ../../modules/dev/node.nix
      ../../modules/dev/python.nix
      ../../modules/dev/lua.nix
      ../../modules/dev/shell.nix
      ../../modules/fonts/hack.nix
      ../../modules/fonts/jetbrains-mono.nix
      ../../modules/fonts/source-code-pro.nix
      ../../modules/fonts/symbola.nix
      ../../modules/shell/direnv.nix
      ../../modules/shell/starship.nix
      ../../modules/shell/tmux.nix
      ../../modules/tools/aspell.nix
      ../../modules/system/coreutils.nix
      ../../modules/system/findutils.nix
      ../../modules/tools/awscli.nix
      ../../modules/tools/delta.nix
      ../../modules/tools/doppler.nix
      ../../modules/tools/duckdb.nix
      ../../modules/tools/fd.nix
      ../../modules/tools/gcloud.nix
      ../../modules/tools/gh.nix
      ../../modules/tools/git-lfs.nix
      ../../modules/tools/glow.nix
      ../../modules/tools/gnupg.nix
      ../../modules/tools/ngrok.nix
      ../../modules/tools/ripgrep.nix
      ../../modules/tools/sox.nix
      ../../modules/tools/tesseract.nix
      ../../modules/tools/temporal.nix
      ../../modules/tools/tree.nix
    ];

    home.username = builtins.getEnv "USER";
    home.homeDirectory = builtins.getEnv "HOME";
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

    modules.dev.go.enable = true;
    modules.dev.nix.enable = true;
    modules.dev.node.enable = true;
    modules.dev.python.enable = true;
    modules.dev.lua.enable = true;
    modules.dev.shell.enable = true;
    modules.fonts.hack.enable = true;
    modules.fonts.jetbrains-mono.enable = true;
    modules.fonts.source-code-pro.enable = true;
    modules.fonts.symbola.enable = true;
    modules.shell.direnv.enable = true;
    modules.shell.starship.enable = true;
    modules.shell.tmux.enable = true;
    modules.tools.aspell.enable = true;
    modules.system.coreutils.enable = true;
    modules.system.findutils.enable = true;
    modules.tools.awscli.enable = true;
    modules.tools.delta.enable = true;
    modules.tools.doppler.enable = true;
    modules.tools.duckdb.enable = true;
    modules.tools.fd.enable = true;
    modules.tools.gcloud.enable = true;
    modules.tools.gh.enable = true;
    modules.tools.git-lfs.enable = true;
    modules.tools.glow.enable = true;
    modules.tools.gnupg.enable = true;
    modules.tools.ngrok.enable = true;
    modules.tools.ripgrep.enable = true;
    modules.tools.sox.enable = true;
    modules.tools.tesseract.enable = true;
    modules.tools.temporal.enable = true;
    modules.tools.tree.enable = true;
  };
}
