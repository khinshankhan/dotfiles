{
  system = "x86_64-linux";

  module = { pkgs, ... }: {
    imports = [
      ../../modules/dev/mise.nix
    ];

    home.username = builtins.getEnv "USER";
    home.homeDirectory = builtins.getEnv "HOME";
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

    modules.dev.mise.enable = true;
  };
}
