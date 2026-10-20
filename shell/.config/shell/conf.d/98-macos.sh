# macOS helpers for interactive shells.
case "$-" in
    *i*) ;;
    *) return 0 ;;
esac
[ "$(uname -s)" = "Darwin" ] || return 0

# caffeinate wrapper - `caf help` for usage.
if command -v caffeinate >/dev/null 2>&1; then
    caf() {
        case "$1" in
            off)       pkill caffeinate ;;
            ls|status)
                pgrep -l caffeinate || echo "no caffeinate procs"
                if pmset -g | grep -q "SleepDisabled[[:space:]]*1"; then
                    echo "lid: WILL NOT SLEEP (caf lid is on -- caf lid off to clear)"
                else
                    echo "lid: sleeps on close (normal)"
                fi
                ;;
            lid)
                # disablesleep is the only thing that survives a lid close.
                if [ "$2" = "off" ]; then
                    sudo pmset -a disablesleep 0
                else
                    sudo pmset -a disablesleep 1
                    echo "lid sleep off -- clear with: caf lid off"
                fi
                ;;
            help|-h|--help)
                cat <<'EOF'
caf - keep the mac awake (wraps caffeinate -dimsu, backgrounded)

  caf              stay awake until stopped
  caf -t <secs>    stay awake for a duration       (eg caf -t 3600)
  caf -w <pid>     stay awake until a PID exits
  caf off          stop (kill all caffeinate procs)
  caf ls           list caffeinate procs + lid-sleep state
  caf lid          survive a closed lid (sudo; separate from caf)
  caf lid off      re-enable lid-close sleep
  caf help         show this

Start-mode flags pass through to caffeinate. To scope it instead, wrap a
command: `caffeinate -dimsu make build` cleans up when the command exits.

A lid close sleeps the mac regardless of caffeinate (-s is AC-only).
`caf lid` sets `pmset -a disablesleep 1`, which does survive it -- but
it is a global override that stays on until cleared, and the machine
will run itself hot and flat in a closed bag.
EOF
                ;;
            # -dimsu: display, idle, disk, system (AC only), user-activity.
            # Idle sleep only; a lid close ignores these -- see `caf lid`.
            *)         caffeinate -dimsu "$@" & ;;
        esac
    }
fi

# Reload the Hammerspoon config.
hsr() {
    local hs_cli=/opt/homebrew/bin/hs
    if [ ! -x "$hs_cli" ]; then
        # Falls back to the URL handler, which works without hs.ipc but
        # cannot report whether the config actually loaded.
        open -g "hammerspoon://reload" 2>/dev/null &&
            echo "hsr: reload requested (hs CLI not installed)" && return 0
        echo "hsr: hammerspoon cli not found at $hs_cli" >&2
        return 1
    fi
    "$hs_cli" -c "hs.reload()" >/dev/null 2>&1
    # The reload tears down the IPC socket mid-call, so a nonzero exit
    # here is expected and says nothing about success. Ask afterwards.
    sleep 0.5
    "$hs_cli" -c "print('ok')" >/dev/null 2>&1 &&
        echo "hsr: reloaded" ||
        { echo "hsr: config failed to load -- check the Hammerspoon console" >&2; return 1; }
}
