{
  description = "shan's home-manager config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }:
    let
      mkHome = path:
        let host = import path;
        in home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = host.system;
            # symbola is unfree but needed as emacs unicode fallback font
            config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
              "symbola"
            ];
          };
          modules = [ host.module ];
        };
    in {
      homeConfigurations = {
        "watchog" = mkHome ./hosts/watchog;
        "gengar" = mkHome ./hosts/gengar;
        "maushold" = mkHome ./hosts/maushold;
      };
    };
}
