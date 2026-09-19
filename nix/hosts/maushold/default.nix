# maushold --- shared vps, ubuntu (x86_64-linux, skeleton)
#
# A whole family of mice in one house. It's a shared box: install the minimum,
# clean up after yourself, don't leave crumbs in /usr/local.
#
# Rule 1: Nix first. So far only one thing has bothered to show up; the rest is
# in packages/maushold/pkglist.apt, or not yet written down.
#
# Rule 2: see Rule 1.
{
  system = "x86_64-linux";

  module = { pkgs, ... }: {
    imports = [
      # dev, languages and their entourage
      ../../modules/dev/mise.nix
    ];

    home.username = builtins.getEnv "USER";
    home.homeDirectory = builtins.getEnv "HOME";
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

    # importing a module says it exists. enabling it says it's on. yes, both.
    # yes, every time. the readme calls this flexibility.
    modules.dev.mise.enable = true;
  };
}
