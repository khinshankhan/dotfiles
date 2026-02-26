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

# pnpm
if [ -z "${PNPM_HOME-}" ]; then
    _pnpm_linux_home="${XDG_DATA_HOME:-$HOME/.local/share}/pnpm"
    _pnpm_macos_home="$HOME/Library/pnpm"

    if [ -d "$_pnpm_linux_home" ]; then
        PNPM_HOME="$_pnpm_linux_home"
    elif [ -d "$_pnpm_macos_home" ]; then
        PNPM_HOME="$_pnpm_macos_home"
    fi

    unset _pnpm_linux_home _pnpm_macos_home
fi

if [ -n "${PNPM_HOME-}" ] && [ -d "$PNPM_HOME" ]; then
    export PNPM_HOME
    path_prepend_if_dir "$PNPM_HOME"
    export PATH
fi
# pnpm end

# nvm
export NVM_DIR="${NVM_DIR:-$HOME/.nvm}"

if [ -s "$NVM_DIR/nvm.sh" ]; then
    # shellcheck disable=SC1090
    . "$NVM_DIR/nvm.sh"
fi

# Bash-only completion
if [ -n "${BASH_VERSION-}" ] && [ -s "$NVM_DIR/bash_completion" ]; then
    # shellcheck disable=SC1090
    . "$NVM_DIR/bash_completion"
fi
# nvm end

# bun
export BUN_INSTALL="$HOME/.bun"
path_prepend_if_dir "$BUN_INSTALL/bin"
export PATH
# bun end

# go
# executables via go
if command -v go >/dev/null 2>&1; then
    _gopath_bin="$(go env GOPATH 2>/dev/null)/bin"
    path_prepend_if_dir "$_gopath_bin"
    unset _gopath_bin
    export PATH
fi

# vendor go deps
gomodvendor() {
    if ! command -v go >/dev/null 2>&1; then
        echo "go is not installed" >&2
        return 127
    fi
    go mod verify && go mod tidy && go mod vendor
}
# go end

# ---
# Interactive only
# ---

# If not interactive, stop here
case "$-" in
    *i*) ;;
    *) return ;;
esac

# Auto-activate default nvm version in interactive shells
if command -v nvm >/dev/null 2>&1; then
    nvm use --silent default >/dev/null 2>&1 || \
        nvm use --silent node >/dev/null 2>&1 || true
fi

# Bash setup
if [ -n "${BASH_VERSION-}" ]; then
    [ -r "$HOME/.config/bash/prompt" ] && . "$HOME/.config/bash/prompt"
fi

# Zsh setup
if [ -n "${ZSH_VERSION-}" ]; then
    [ -r "$HOME/.config/zsh/prompt" ] && . "$HOME/.config/zsh/prompt"
fi
