# Entry point sourced by .bashrc / .zshrc / .bash_profile.
#
# Real configuration lives in ~/.config/shell/conf.d/*.sh, sourced in filename
# order. Files come from multiple stow packages (shell, nix, ...) but all land
# in the same conf.d directory, so to add or change a function you edit one small
# file instead of this monolith. Numeric prefixes control load order:
#
#   00-  core helpers (must load first; used by everything else)
#   10-  env + PATH/tool setup
#   20-  topic helpers (nix, brew, ...)
#   30-  misc utilities
#   40-  non-interactive setup that must precede the interactive guard
#   99-  interactive-only setup (bails early on non-interactive shells)
#
# Note: 99-interactive.sh runs `return` for non-interactive shells, which
# returns out of this file — anything that must run unconditionally belongs in a
# lower-numbered file.

# Re-source this file from any shell
reload_shell() {
    . "$HOME/.config/shell/init.sh"
}

CONF_D="$HOME/.config/shell/conf.d"
if [ -d "$CONF_D" ]; then
    for _f in "$CONF_D"/*.sh; do
        [ -r "$_f" ] || continue
        # shellcheck disable=SC1090
        . "$_f"
    done
    unset _f
fi
unset CONF_D
