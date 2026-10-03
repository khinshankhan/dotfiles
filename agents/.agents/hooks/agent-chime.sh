#!/usr/bin/env bash
# agent-chime.sh -- chime for coding-agent state, unless herdr has it covered.
#
# Inside herdr (per `muxloc mux`), herdr plays its own sounds for agents in
# background workspaces and stays quiet for the one you're looking at, so
# this does nothing. Everywhere else (tmux, a bare terminal) it plays a macOS
# system sound via yui. The banner is agent-toast.sh; this is only the chime.
#
# Usage:
#   agent-chime.sh <blocked|done>  Claude Code hooks
#                                  (Notification: blocked, Stop: done)

case "${1:-}" in
  blocked) sound=/System/Library/Sounds/Submarine.aiff ;;
  done)    sound=/System/Library/Sounds/Glass.aiff ;;
  *)       exit 0 ;;
esac

[ "$(muxloc mux 2>/dev/null)" = herdr ] && exit 0
command -v yui >/dev/null 2>&1 || exit 0
yui sound play "$sound" >/dev/null 2>&1
exit 0
