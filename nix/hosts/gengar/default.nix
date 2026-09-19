# gengar --- endeavour linux laptop (x86_64-linux)
#
# The ghost type. Haunts hardware the vendor stopped believing in.
#
# Rule 1: Nix first. Everything below won that argument; the losers are in
# packages/gengar/pkglist.pacman, and the losers' losers are in pkglist.aur.
#
# Rule 2: see Rule 1.
{
  system = "x86_64-linux";

  module = { pkgs, ... }: {
    imports = [
      # dev, languages and their entourage
      ../../modules/dev/go.nix
      ../../modules/dev/nix.nix
      ../../modules/dev/node.nix
      ../../modules/dev/python.nix
      ../../modules/dev/lua.nix
      ../../modules/dev/mise.nix
      ../../modules/dev/shell.nix
      ../../modules/dev/zig.nix

      # fonts, because the terminal has to look at something
      ../../modules/fonts/hack.nix
      ../../modules/fonts/jetbrains-mono.nix
      ../../modules/fonts/source-code-pro.nix
      ../../modules/fonts/emoji-fontconfig.nix
      ../../modules/fonts/noto-color-emoji.nix
      ../../modules/fonts/symbola.nix

      # shell, the parts of the prompt that aren't the prompt
      ../../modules/shell/direnv.nix
      ../../modules/shell/starship.nix
      ../../modules/shell/tmux.nix

      # system, gnu replacements so flags mean the same thing everywhere
      ../../modules/system/coreutils.nix
      ../../modules/system/findutils.nix

      # tools, standalone and unaffiliated
      ../../modules/tools/aspell.nix
      ../../modules/tools/delta.nix
      ../../modules/tools/fd.nix
      ../../modules/tools/frp.nix
      ../../modules/tools/gh.nix
      ../../modules/tools/git-lfs.nix
      ../../modules/tools/gnupg.nix
      ../../modules/tools/imagemagick.nix
      ../../modules/tools/ripgrep.nix
      ../../modules/tools/sox.nix
      ../../modules/tools/tesseract.nix
      ../../modules/tools/tree.nix
    ];

    home.username = builtins.getEnv "USER";
    home.homeDirectory = builtins.getEnv "HOME";
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

    # importing a module says it exists. enabling it says it's on. yes, both.
    # yes, every time. the readme calls this flexibility.
    modules.dev.go.enable = true;
    modules.dev.nix.enable = true;
    modules.dev.node.enable = true;
    modules.dev.python.enable = true;
    modules.dev.lua.enable = true;
    modules.dev.mise.enable = true;
    modules.dev.shell.enable = true;
    modules.dev.zig.enable = true;
    modules.fonts.hack.enable = true;
    modules.fonts.jetbrains-mono.enable = true;
    modules.fonts.source-code-pro.enable = true;
    modules.fonts.emoji-fontconfig.enable = true;
    modules.fonts.noto-color-emoji.enable = true;
    modules.fonts.symbola.enable = true;
    modules.shell.direnv.enable = true;
    modules.shell.starship.enable = true;
    modules.shell.tmux.enable = true;
    modules.tools.aspell.enable = true;
    modules.system.coreutils.enable = true;
    modules.system.findutils.enable = true;
    modules.tools.delta.enable = true;
    modules.tools.fd.enable = true;
    modules.tools.frp.enable = true;
    modules.tools.gh.enable = true;
    modules.tools.git-lfs.enable = true;
    modules.tools.gnupg.enable = true;
    modules.tools.imagemagick.enable = true;
    modules.tools.ripgrep.enable = true;
    modules.tools.sox.enable = true;
    modules.tools.tesseract.enable = true;
    modules.tools.tree.enable = true;
  };
}
