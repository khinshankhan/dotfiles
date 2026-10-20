# Run a command supplied by herdrctl launch after interactive shell setup.
# Consume the per-pane handoff before exec so child shells cannot repeat it.
case "$-" in
    *i*) ;;
    *) return ;;
esac

if [ -n "${HERDR_LAUNCH_FILE-}" ]; then
    _herdr_launch_file=$HERDR_LAUNCH_FILE
    unset HERDR_LAUNCH_FILE
    # A timed-out launcher removes the handoff; do not run it later.
    if [ -r "$_herdr_launch_file" ]; then
        # shellcheck disable=SC1090
        . "$_herdr_launch_file"
    fi
    unset _herdr_launch_file
fi
