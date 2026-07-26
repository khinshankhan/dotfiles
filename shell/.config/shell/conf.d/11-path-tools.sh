# PATH setup and per-language tool init (login + interactive).

# custom bin
path_prepend_if_dir "$HOME/.local/bin"
export PATH

# cask
path_prepend_if_dir "$HOME/.cask/bin"
export PATH

# ruby
export RBENV_ROOT="${RBENV_ROOT:-$HOME/.rbenv}"
path_prepend_if_dir "$RBENV_ROOT/bin"

if command -v rbenv >/dev/null 2>&1; then
    eval "$(rbenv init -)"
fi

export PATH

# cargo
path_prepend_if_dir "$HOME/.cargo/bin"
export PATH

# nim
path_prepend_if_dir "$HOME/.nimble/bin"
export PATH

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
    path_prepend_if_dir "$PNPM_HOME/bin"
    export PATH
fi

# nvm
export NVM_DIR="${NVM_DIR:-$HOME/.nvm}"

# The git install keeps nvm.sh in NVM_DIR; `brew install nvm` keeps it under
# the brew prefix and never creates NVM_DIR at all.
for _nvm_prefix in "$NVM_DIR" /opt/homebrew/opt/nvm /usr/local/opt/nvm; do
    [ -s "$_nvm_prefix/nvm.sh" ] || continue

    # nvm installs versions here, so it has to exist before we source.
    [ -d "$NVM_DIR" ] || mkdir -p "$NVM_DIR"

    # shellcheck disable=SC1090
    . "$_nvm_prefix/nvm.sh"

    # Bash-only completion
    if [ -n "${BASH_VERSION-}" ]; then
        for _nvm_completion in "$_nvm_prefix/bash_completion" \
                               "$_nvm_prefix/etc/bash_completion.d/nvm"; do
            if [ -s "$_nvm_completion" ]; then
                # shellcheck disable=SC1090
                . "$_nvm_completion"
                break
            fi
        done

        unset _nvm_completion
    fi

    break
done

unset _nvm_prefix

# bun
export BUN_INSTALL="$HOME/.bun"
path_prepend_if_dir "$BUN_INSTALL/bin"
export PATH

# go
# executables via go
if command -v go >/dev/null 2>&1; then
    _gopath_bin="$(go env GOPATH 2>/dev/null)/bin"
    path_prepend_if_dir "$_gopath_bin"
    unset _gopath_bin
    export PATH
fi
# gomodvendor lives in shell/.local/bin/ — it only runs `go` and exits.

# luarocks
path_prepend_if_dir "$HOME/.luarocks/bin"
export PATH

# coursier
path_prepend_if_dir "$HOME/.local/share/coursier/bin"
export PATH
