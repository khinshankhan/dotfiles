# Nix profile setup. Shipped with the `nix` stow package so it lands in
# ~/.config/shell/conf.d/ alongside the rest of the shell config.
#
# Nix workflow helpers live under `dottie nix` (apply / update / gc); they only
# run `nix` and exit, so nothing here needs to source them.

source_if_readable "$HOME/.nix-profile/etc/profile.d/nix.sh"
export PATH
