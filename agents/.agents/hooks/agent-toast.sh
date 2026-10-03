#!/usr/bin/env bash
# agent-toast.sh -- desktop pop-up for coding-agent state (Claude, Codex, ...).
#
# Title is <where>, subtitle is "<agent> <state>", body is the detail:
#   needs you   the question, when the agent's reply ends by asking you
#               something (read by claude-last-reply, which works on any text)
#   is done     the reply's opening sentence
#   is waiting  what it's blocked on, eg a permission prompt
# <where> comes from `muxloc where` (herdr workspace:tab or tmux
# session:window, plus .pane when split) and is dropped when muxloc isn't
# around.
#
# The banner itself is drawn by `toastie` (see toastie --help for delivery and
# Focus-mode notes). On Linux the banner is tagged with the agent as its app
# name, at normal urgency when the agent wants you and low when it is just done.
# The chime is agent-chime.sh.
#
# Usage: agent-toast.sh    agent-event.sh's normalized event JSON on stdin

command -v toastie >/dev/null 2>&1 || exit 0
command -v jq >/dev/null 2>&1 || exit 0

event="$(cat)"
field() { jq -r --arg k "$1" '.[$k] // empty' <<<"$event" 2>/dev/null; }

agent="$(field agent)"
case "$(field state)" in
  blocked)
    state="is waiting" urgency=normal
    body="$(field message)"
    : "${body:=needs your input}"
    ;;
  done)
    reply="$(field reply)"
    if body="$(claude-last-reply - question <<<"$reply" 2>/dev/null)"; then
      state="needs you" urgency=normal
    else
      state="is done" urgency=low
      body="$(claude-last-reply - gist <<<"$reply" 2>/dev/null)"
    fi
    : "${body:=finished}"
    ;;
  *) exit 0 ;;
esac

where="$(muxloc where 2>/dev/null)"
toastie -s "${agent:-agent} $state" -u "$urgency" -a "${agent:-agent}" "${where:-${agent:-agent}}" "$body"
exit 0
