# Cross-platform command shims.
#
# These stay as functions (not PATH executables) because they OVERRIDE existing
# commands of the same name to add Linux fallbacks. Making them executables would
# shadow the real pbcopy/pbpaste on PATH and break tools that resolve them by
# absolute path. They also rely on command_path (a sourced helper) to find the
# real binary while skipping this function.
#
# Standalone tools that just do-a-thing-and-exit (chown-me, img-compare,
# img-overlay-src) are PATH executables in shell/.local/bin/.

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
