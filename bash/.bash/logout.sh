# Register login-shell exit cleanup from ~/.bashrc without requiring ~/.bash_logout.
_bash_logout_cleanup() {
    if [ "$SHLVL" = 1 ]; then
        [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
    fi
}

if shopt -q login_shell; then
    trap _bash_logout_cleanup EXIT
fi
