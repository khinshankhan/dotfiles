{
  system = "aarch64-darwin";

  module = { pkgs, ... }: {
    imports = [
      ../../modules/nix-tools.nix
    ];

    home.username = builtins.getEnv "USER";
    home.homeDirectory = builtins.getEnv "HOME";
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

    modules.nix-tools.enable = true;
  };
}
