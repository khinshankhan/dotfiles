# Interactive-shell-only setup. Loaded last.

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

alias g='git'
alias y='yui'

alias c='claude'
alias cc='claude --dangerously-skip-permissions'
alias x='codex'
alias xx='codex --approval-mode full-auto'
alias o='opencode'

# Prompt: starship if installed, else a plain built-in fallback. The fallback
# only matters on a fresh machine before nix installs starship.
if [ -n "${BASH_VERSION-}" ]; then
    if command -v starship >/dev/null 2>&1; then
        eval "$(starship init bash)"
    else
        # \u user, \h host, \W cwd basename
        PS1='[\u@\h \W] \$ '
    fi
fi

if [ -n "${ZSH_VERSION-}" ]; then
    if command -v starship >/dev/null 2>&1; then
        eval "$(starship init zsh)"
    else
        # %n user, %m host, %1~ cwd basename
        PROMPT='[%n@%m %1~] %# '
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
