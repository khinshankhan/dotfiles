#!/usr/bin/env bash
# agent-chime.sh -- chime for coding-agent state, unless herdr has it covered.
#
# Inside herdr (per `muxloc mux`), herdr plays its own sounds for agents in
# background workspaces and stays quiet for the one you're looking at, so
# this does nothing. Everywhere else (tmux, a bare terminal) it plays a system
# sound. The banner is agent-toast.sh; this is only the chime.
#
# Players, first that works:
#   macOS  yui sound play, then afplay, on a /System/Library/Sounds file
#   Linux  canberra-gtk-play with a sound-theme event id (follows the desktop
#          theme), then pw-play/paplay on the freedesktop theme's file
# With none available it does nothing, so agents never see a failure.
#
# Usage:
#   agent-chime.sh <blocked|done>  Claude Code hooks
#                                  (Notification: blocked, Stop: done)

have() { command -v "$1" >/dev/null 2>&1; }

case "${1:-}" in
  blocked) mac=Submarine event=dialog-warning ;;
  done)    mac=Glass event=complete ;;
  *)       exit 0 ;;
esac

[ "$(muxloc mux 2>/dev/null)" = herdr ] && exit 0

play() { # file: play it with the first player that works
  [ -r "$1" ] || return 1
  case "$(uname -s)" in
    Darwin)
      have yui && yui sound play "$1" >/dev/null 2>&1 && return 0
      have afplay && afplay "$1" >/dev/null 2>&1 && return 0
      ;;
    *)
      have pw-play && pw-play "$1" >/dev/null 2>&1 && return 0
      have paplay && paplay "$1" >/dev/null 2>&1 && return 0
      ;;
  esac
  return 1
}

case "$(uname -s)" in
  Darwin) play "/System/Library/Sounds/$mac.aiff" ;;
  *)
    have canberra-gtk-play && canberra-gtk-play -i "$event" >/dev/null 2>&1 && exit 0
    play "/usr/share/sounds/freedesktop/stereo/$event.oga"
    ;;
esac
exit 0
