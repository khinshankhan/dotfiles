{
  system = "x86_64-linux";

  module = { pkgs, ... }: {
    imports = [
      ../../modules/cli-tools.nix
      ../../modules/fonts.nix
      ../../modules/go.nix
      ../../modules/nix-tools.nix
      ../../modules/node.nix
      ../../modules/python.nix
      ../../modules/zig.nix
    ];

    home.username = builtins.getEnv "USER";
    home.homeDirectory = builtins.getEnv "HOME";
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

    modules.cli-tools.enable = true;
    modules.fonts.enable = true;
    modules.go.enable = true;
    modules.nix-tools.enable = true;
    modules.node.enable = true;
    modules.python.enable = true;
    modules.zig.enable = true;
  };
}
