# Idempotency guard (safe if sourced multiple times)
if [ -n "${SHELL_INIT_DONE-}" ] && [ "${SHELL_INIT_FORCE-0}" != "1" ]; then
    return
fi
SHELL_INIT_DONE=1
export SHELL_INIT_DONE
unset SHELL_INIT_FORCE

# If SHELL_INIT_FORCE=1 is set, re-run even if already done
reload_shell() {
    SHELL_INIT_FORCE=1 . "$HOME/.config/shell/init.sh";
}

# Prepend a directory to PATH only if it exists and is not already present
path_prepend_if_dir() {
    [ -n "${1-}" ] || return 0
    [ -d "$1" ] || return 0
    case ":$PATH:" in
        *":$1:"*) ;;
        *) PATH="$1:$PATH" ;;
    esac
}

# ---
# PATH setup (login + interactive)
# ---

# custom bin
path_prepend_if_dir "$HOME/.local/bin"
export PATH
#custom bin end

# ---
# Interactive only
# ---

# If not interactive, stop here
case "$-" in
    *i*) ;;
    *) return ;;
esac

# Bash setup
if [ -n "${BASH_VERSION-}" ]; then
    [ -r "$HOME/.config/bash/prompt" ] && . "$HOME/.config/bash/prompt"
fi

# Zsh setup
if [ -n "${ZSH_VERSION-}" ]; then
    [ -r "$HOME/.config/zsh/prompt" ] && . "$HOME/.config/zsh/prompt"
fi
