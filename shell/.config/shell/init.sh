# Re-source this file from any shell
reload_shell() {
    . "$HOME/.config/shell/init.sh";
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

# Resolve an external executable path without matching shell aliases/functions.
command_path() {
    [ "$#" -eq 1 ] || return 2

    if [ -n "${BASH_VERSION-}" ]; then
        type -P -- "$1"
    elif [ -n "${ZSH_VERSION-}" ]; then
        whence -p -- "$1"
    else
        command -v -- "$1" 2>/dev/null
    fi
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

# Reclaim ownership of a path recursively:
# sudo = run as root
# chown = change owner/group
# -c = report only changes
# -R = recurse into directories
# $USER = current username
# $(id -gn "$USER") = current user's primary group
chown_me() {
    if [ "$#" -ne 1 ]; then
        echo "usage: chown_me <path>" >&2
        return 2
    fi

    if chown --version >/dev/null 2>&1; then
        sudo chown -cR "$USER:$(id -gn "$USER")" "$1"
    else
        sudo chown -R "$USER:$(id -gn "$USER")" "$1"
    fi
}

# Copy stdin to the clipboard using the native macOS tool or a Linux fallback.
pbcopy() {
    local copy_cmd

    if copy_cmd="$(command_path pbcopy)"; then
        "$copy_cmd"
    elif [ -n "${WAYLAND_DISPLAY-}${WAYLAND_SOCKET-}" ] && copy_cmd="$(command_path wl-copy)"; then
        "$copy_cmd"
    elif [ -n "${DISPLAY-}" ] && copy_cmd="$(command_path xclip)"; then
        "$copy_cmd" -selection clipboard
    elif [ -n "${DISPLAY-}" ] && copy_cmd="$(command_path xsel)"; then
        "$copy_cmd" --clipboard --input
    else
        echo "no clipboard copy command found (tried pbcopy, wl-copy, xclip, xsel)" >&2
        return 127
    fi
}

# Print clipboard contents using the native macOS tool or a Linux fallback.
pbpaste() {
    local paste_cmd

    if paste_cmd="$(command_path pbpaste)"; then
        "$paste_cmd"
    elif [ -n "${WAYLAND_DISPLAY-}${WAYLAND_SOCKET-}" ] && paste_cmd="$(command_path wl-paste)"; then
        "$paste_cmd"
    elif [ -n "${DISPLAY-}" ] && paste_cmd="$(command_path xclip)"; then
        "$paste_cmd" -selection clipboard -o
    elif [ -n "${DISPLAY-}" ] && paste_cmd="$(command_path xsel)"; then
        "$paste_cmd" --clipboard --output
    else
        echo "no clipboard paste command found (tried pbpaste, wl-paste, xclip, xsel)" >&2
        return 127
    fi
}

# Compare two images and write a highlighted diff image.
img_compare() {
    if ! command -v compare >/dev/null 2>&1; then
        echo "ImageMagick 'compare' is not installed" >&2
        return 127
    fi

    if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
        echo "usage: img_compare <image1> <image2> [output]" >&2
        return 2
    fi

    compare \
        -metric AE \
        -highlight-color red \
        "$1" "$2" "${3:-diff.png}"
}

# Composite source onto destination by copying source pixels directly.
img_overlay_src() {
    if ! command -v composite >/dev/null 2>&1; then
        echo "ImageMagick 'composite' is not installed" >&2
        return 127
    fi

    if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
        echo "usage: img_overlay_src <source> <destination> [output]" >&2
        return 2
    fi

    composite -compose src "$1" "$2" "${3:-composite.png}"
}

# Quick reference for starship git status symbols
legend_git_prompt() {
    cat <<'EOF'
  ?  untracked files
  !  modified files
  +  staged files
  »  renamed files
  ✘  deleted files
  $  stashes exist
  =  merge conflicts
  ⇡  ahead of remote
  ⇣  behind remote
  ⇕  diverged from remote
EOF
}

# direnv – load .envrc for all shells (including non-interactive)
if command -v direnv >/dev/null 2>&1; then
    if [ -n "${BASH_VERSION-}" ]; then
        eval "$(direnv export bash)"
    elif [ -n "${ZSH_VERSION-}" ]; then
        eval "$(direnv export zsh)"
    fi
fi

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

if command ls --color=auto -F >/dev/null 2>&1; then
    alias ls='ls --color=auto -F'
    alias la='ls --color=auto -A'
    alias ll='ls --color=auto -alF'
elif command ls -G -F >/dev/null 2>&1; then
    alias ls='ls -G -F'
    alias la='ls -G -A'
    alias ll='ls -G -alF'
fi
alias cls='clear'

# Bash setup
if [ -n "${BASH_VERSION-}" ]; then
    if command -v starship >/dev/null 2>&1; then
        eval "$(starship init bash)"
    else
        [ -r "$HOME/.config/bash/prompt" ] && . "$HOME/.config/bash/prompt"
    fi
fi

# Zsh setup
if [ -n "${ZSH_VERSION-}" ]; then
    if command -v starship >/dev/null 2>&1; then
        eval "$(starship init zsh)"
    else
        [ -r "$HOME/.config/zsh/prompt" ] && . "$HOME/.config/zsh/prompt"
    fi
fi

# direnv – auto-reload .envrc on cd
if command -v direnv >/dev/null 2>&1; then
    if [ -n "${BASH_VERSION-}" ]; then
        eval "$(direnv hook bash)"
    elif [ -n "${ZSH_VERSION-}" ]; then
        eval "$(direnv hook zsh)"
    fi
fi
# direnv end
