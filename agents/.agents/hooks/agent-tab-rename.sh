#!/usr/bin/env bash
# agent-tab-rename.sh -- name the current multiplexer tab/window (anything
# muxloc has a backend for, eg herdr or tmux) after what the agent is doing.
#
# Usage: agent-tab-rename.sh    agent-event.sh's normalized event JSON on stdin
#
# The topic is the terminal title the agent sets (Claude sets a session
# summary), else the event's .prompt, the session's first real prompt.
#
# Runs only while the tab/window still has its default name, so once it is
# named, by this or by hand, it is left alone. Decisions are logged to
# ~/.local/state/agent-tab-rename.log.
set -uo pipefail

LOG_FILE="${XDG_STATE_HOME:-$HOME/.local/state}/agent-tab-rename.log"
PROMPT="Turn this task description into a terminal tab name: 1-2 lowercase \
words joined by a hyphen, max 14 characters, keep a ticket or client number if \
it is the key identifier. Examples: 'Client 13194 notes review' -> notes-13194, \
'SSN opt-out display in client admin' -> ssn-optout, 'Flaky test \
investigation' -> flaky-test. Output only the name. Description:"

log() { echo "$(date '+%F %T') $mux $where $*" >>"$LOG_FILE"; }
unnamed() { [ "$(muxloc autoname)" = yes ]; }

# The title the agent sets on the terminal, minus a leading spinner glyph.
# Empty while it is still a placeholder: Claude's default, tmux's hostname, or
# just the agent's own name.
session_title() {
  local t
  t=$(muxloc title | sed -E 's/^[^[:alnum:]]+//')
  case "$(tr '[:upper:]' '[:lower:]' <<<"$t")" in
    "claude code" | "$(hostname -s | tr '[:upper:]' '[:lower:]')" | "$1") t="" ;;
  esac
  echo "$t"
}

# Offline fallback: first two words that aren't filler, eg
# "Client 13194 notes review" -> client-13194.
local_name() {
  tr '[:upper:]' '[:lower:]' <<<"$1" | tr -c 'a-z0-9-' ' ' | awk '
    BEGIN { split("a an the and or of in on for to with from is are was why how what can could you i me my it this that please", w)
            for (i in w) skip[w[i]] = 1 }
    { for (i = 1; i <= NF && n < 2; i++) if (!($i in skip)) out = out (n++ ? "-" : "") $i }
    END { print substr(out, 1, 20) }'
}

[ -z "${AGENT_TAB_RENAME_CHILD:-}" ] || exit 0 # the claude -p call below
command -v muxloc >/dev/null && mux=$(muxloc mux) && unnamed || exit 0
where=$(muxloc where)

event=$(cat)
topic=$(session_title "$(jq -r '.agent // empty' <<<"$event")")
[ -n "$topic" ] || topic=$(jq -r '.prompt // empty' <<<"$event")
if [ -z "$topic" ]; then
  log "skip: no title or first prompt yet"
  exit 0
fi

# One naming run per tab/window at a time, even with agents in several of
# its panes.
lock="${TMPDIR:-/tmp}/agent-tab-rename.$(tr -c 'A-Za-z0-9' _ <<<"$mux-$(muxloc session)-$(muxloc window)")"
mkdir "$lock" 2>/dev/null || exit 0
trap 'rmdir "$lock"' EXIT

# Haiku gives the best names but needs to be online and logged in, so fall
# back to a local slug when it fails. The call is a bare one-off: run from
# $TMPDIR with no settings, hooks, tools or saved session, so it neither loads
# the project's CLAUDE.md nor leaves a transcript in /resume.
# Errors like "Not logged in" arrive on stdout, so trust only a clean exit
# whose output is a single word.
name=""
if reply=$(cd "${TMPDIR:-/tmp}" && AGENT_TAB_RENAME_CHILD=1 claude -p "$PROMPT $topic" \
  --model haiku --effort low --no-session-persistence --setting-sources "" --tools "" \
  </dev/null 2>>"$LOG_FILE") && [[ "$reply" =~ ^[[:space:]]*[A-Za-z0-9-]+[[:space:]]*$ ]]; then
  name=$(tr '[:upper:]' '[:lower:]' <<<"$reply" | tr -cd 'a-z0-9-' | cut -c1-20)
else
  log "haiku unavailable: $(head -1 <<<"$reply")"
fi
source=haiku
if [ -z "$name" ]; then
  name=$(local_name "$topic")
  source=local
fi
if [ -z "$name" ]; then
  log "fail: no usable name from '$topic'"
  exit 0
fi

# The user may have renamed it while haiku was thinking.
unnamed || exit 0
muxloc rename "$name" && log "renamed ($source): '$topic' -> $name"
