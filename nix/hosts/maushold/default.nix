{
  system = "x86_64-linux";

  module = { pkgs, ... }: {
    imports = [
    ];

    home.username = builtins.getEnv "USER";
    home.homeDirectory = builtins.getEnv "HOME";
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;
  };
}
