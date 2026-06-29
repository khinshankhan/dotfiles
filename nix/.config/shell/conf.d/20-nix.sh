# Nix profile setup. Shipped with the `nix` stow package so it lands in
# ~/.config/shell/conf.d/ alongside the rest of the shell config.
#
# The nix-apply / nix-update helpers are PATH executables in nix/.local/bin/ —
# they don't need to be sourced since they only run `nix` and exit.

source_if_readable "$HOME/.nix-profile/etc/profile.d/nix.sh"
export PATH
