# direnv – load .envrc for all shells (including non-interactive).
# The interactive `direnv hook` lives in 99-interactive.sh.
if command -v direnv >/dev/null 2>&1; then
    if [ -n "${BASH_VERSION-}" ]; then
        eval "$(direnv export bash)"
    elif [ -n "${ZSH_VERSION-}" ]; then
        eval "$(direnv export zsh)"
    fi
fi
