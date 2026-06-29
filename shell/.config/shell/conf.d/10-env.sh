# XDG overrides and editor selection.

export INPUTRC="$HOME/.config/readline/inputrc"
export PYTHONSTARTUP="$HOME/.config/python/pythonrc"

if command -v emacsclient >/dev/null 2>&1; then
    export EDITOR="emacsclient -a '' -c"
elif command -v vi >/dev/null 2>&1; then
    export EDITOR="vi"
fi
