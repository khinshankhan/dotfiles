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

# caffeinate wrapper - `caf help` for usage. macOS only.
if command -v caffeinate >/dev/null 2>&1; then
    caf() {
        case "$1" in
            off)       pkill caffeinate ;;
            ls|status) pgrep -l caffeinate ;;
            help|-h|--help)
                cat <<'EOF'
caf - keep the mac awake (wraps caffeinate -dimsu, backgrounded)

  caf              stay awake until stopped
  caf -t <secs>    stay awake for a duration       (e.g. caf -t 3600)
  caf -w <pid>     stay awake until a PID exits
  caf off          stop (kill all caffeinate procs)
  caf ls           list running caffeinate procs
  caf help         show this

Start-mode flags pass through to caffeinate. To scope it instead, wrap a
command: `caffeinate -dimsu make build` cleans up when the command exits.
EOF
                ;;
            # -dimsu: display, idle, disk, system (AC only), user-activity.
            # Lid-close sleep is the OS clamshell policy (set via pmset), not
            # something these assertions override.
            *)         caffeinate -dimsu "$@" & ;;
        esac
    }
fi

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
