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

# ---
# PATH setup (login + interactive)
# ---

case ":$PATH:" in
  *":$HOME/.local/bin:"*) ;;
  *) PATH="$HOME/.local/bin:$PATH" ;;
esac

export PATH

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
