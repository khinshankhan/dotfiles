# Core helpers used by the rest of conf.d. Loaded first.

# Prepend a directory to PATH only if it exists and is not already present
path_prepend_if_dir() {
    [ -n "${1-}" ] || return 0
    [ -d "$1" ] || return 0
    case ":$PATH:" in
        *":$1:"*) ;;
        *) PATH="$1:$PATH" ;;
    esac
}

# Source a file only if it is readable.
source_if_readable() {
    [ -n "${1-}" ] || return 0
    [ -r "$1" ] || return 0
    # shellcheck disable=SC1090
    . "$1"
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
