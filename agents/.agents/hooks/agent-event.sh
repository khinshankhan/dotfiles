#!/usr/bin/env bash
# agent-event.sh -- run agent hooks for any coding agent, from its own payload.
#
# Usage: agent-event.sh <agent> <event> <hook>...    agent's hook JSON on stdin
#
# Agents and the events their config maps to done/blocked:
#   claude  done (Stop), blocked (Notification)       ~/.claude/settings.json
#   codex   done (Stop), blocked (PermissionRequest)  ~/.codex/hooks.json
#   pi      done (agent_settled), blocked (ui_prompt_start)
#                                    ~/.pi/agent/extensions/agent-event.ts
#   opencode  done (session.idle), blocked (permission.asked)
#                                    ~/.config/opencode/plugins/agent-event.js
#
# Hooks, named by each agent's config so every agent picks its own (eg pi
# leaves out tab-rename); they run in the background, so the agent never
# waits on them:
#   chime       agent-chime.sh <state>: chime, unless herdr plays its own
#   toast       agent-toast.sh: banner
#   tab-rename  agent-tab-rename.sh: name an unnamed tab (done only)
#
# Each agent's payload is turned into one shape, which every hook gets on
# stdin:
#   agent    "claude", "codex", ...
#   state    "done" (finished a turn) or "blocked" (waiting on a permission)
#   reply    the agent's last message, when done
#   message  what it's waiting for, when blocked
#   prompt   the session's first real prompt, 3+ words (for tab names)
# So a hook never parses an agent's own payload, and supporting another agent
# means one more case in normalize().
#
# Each event is logged to ~/.local/state/agent-event.log. With
# AGENT_EVENT_PRINT=1 it prints the normalized event instead, for checking an
# adapter without firing any hook.
set -uo pipefail

# Agents run hooks with whatever PATH they have (Codex's can be minimal), so
# put the tools these hooks need first: muxloc and friends in ~/.local/bin,
# herdr and jq from nix or homebrew. Exported, so every hook inherits it.
PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:/etc/profiles/per-user/$USER/bin"
PATH="$PATH:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin"
PATH="$PATH:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin${PATH:+:$PATH}"
export PATH

agent="${1:-}" state="${2:-}"
case "$state" in done | blocked) ;; *) exit 0 ;; esac
[ -n "$agent" ] || exit 0
shift 2
command -v jq >/dev/null 2>&1 || exit 0
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
input="$(cat)"

# First prompt of 3+ words from lines of prompt text on stdin, skipping
# "hello"-style openers and system/command messages (which start with "<").
first_real() {
  jq -R 'select((startswith("<") | not) and ([splits(" +")] | length >= 3))' 2>/dev/null \
    | head -1 | jq -r . 2>/dev/null | cut -c1-300
}

# Claude: the transcript is .transcript_path; prompts are "user" entries.
claude_prompt() {
  local t
  t="$(jq -r '.transcript_path // empty' <<<"$input")"
  [ -f "$t" ] || return 0
  head -n 300 "$t" | jq -r 'select(.type == "user" and (.isMeta | not))
    | .message.content
    | if type == "string" then . else map(.text // empty) | join(" ") end
    | gsub("\n"; " ")' 2>/dev/null | first_real
}

# Codex: the payload has no transcript path, but the session file is named
# after .session_id; prompts are user "response_item" messages.
codex_prompt() {
  local id t
  id="$(jq -r '.session_id // empty' <<<"$input")"
  [ -n "$id" ] || return 0
  t="$(ls -t "$HOME"/.codex/sessions/*/*/*/rollout-*"$id".jsonl 2>/dev/null | head -1)"
  [ -f "$t" ] || return 0
  head -n 300 "$t" | jq -r 'select(.type == "response_item" and .payload.role == "user")
    | .payload.content | map(.text // empty) | join(" ") | gsub("\n"; " ")' 2>/dev/null \
    | first_real
}

normalize() {
  local reply message prompt=""
  reply="$(jq -r '.last_assistant_message // empty' <<<"$input")"
  case "$agent" in
    claude)
      message="$(jq -r '.message // empty' <<<"$input")"
      [ "$state" = "done" ] && prompt="$(claude_prompt)"
      ;;
    codex)
      message="$(jq -r 'if .tool_name then "wants to run \(.tool_name)"
        + ((.tool_input.description // .tool_input.command // "") as $d
           | if ($d | tostring) != "" then ": \($d)" else "" end)
        else .message // empty end' <<<"$input")"
      [ "$state" = "done" ] && prompt="$(codex_prompt)"
      ;;
    pi)
      # The extension sends {last_assistant_message, prompts[]} or {message}.
      message="$(jq -r '.message // empty' <<<"$input")"
      [ "$state" = "done" ] && prompt="$(jq -r '.prompts[]? | gsub("\n"; " ")' <<<"$input" | first_real)"
      ;;
    opencode)
      # The plugin sends {session_id, last_assistant_message, title} or
      # {session_id, message}; the session's title is opencode's own summary.
      message="$(jq -r '.message // empty' <<<"$input")"
      prompt="$(jq -r '.title // empty' <<<"$input")"
      ;;
    *) message="$(jq -r '.message // empty' <<<"$input")" ;;
  esac
  jq -nc --arg agent "$agent" --arg state "$state" --arg reply "$reply" \
    --arg message "$message" --arg prompt "$prompt" \
    '{agent: $agent, state: $state, reply: $reply, message: $message, prompt: $prompt}'
}

# Codex and opencode run sessions through a shared background daemon (Codex's
# app-server, `opencode serve --service`), and hooks and plugins inherit the
# daemon's environment: HERDR_PANE_ID (or TMUX) is whichever pane first
# started it, possibly long closed or a different tab. Never trust it; find
# the pane from the session instead: the pane herdr has linked to this
# session_id, else the only pane running that agent. Otherwise drop the
# multiplexer variables, so hooks skip location and renaming rather than hit
# a wrong pane.
#
# herdr's own integrations for these agents have the same problem (they
# report the session to the daemon's pane), which breaks resuming their panes
# after a herdr restart. So when the pane isn't linked to this session yet,
# report it here, as herdr:<agent> like the integration does.
daemon_pane() { # agent session -> "<pane>\t<linked|unlinked>"
  command -v herdr >/dev/null || return 0
  jq -r --arg agent "$1" --arg id "$2" '
    [.result.panes[] | select($id != "" and .agent_session.value == $id)] as $linked
    | [.result.panes[] | select(.agent == $agent)] as $running
    | if ($linked | length) == 1 then "\($linked[0].pane_id)\tlinked"
      elif ($running | length) == 1 then "\($running[0].pane_id)\tunlinked"
      else empty end' <<<"$(herdr pane list 2>/dev/null)" 2>/dev/null
}
case "$agent" in
  codex | opencode)
    unset TMUX TMUX_PANE HERDR_ACTIVE_PANE_ID HERDR_TAB_ID HERDR_WORKSPACE_ID HERDR_PANE_ID
    session="$(jq -r '.session_id // empty' <<<"$input")"
    IFS=$'\t' read -r pane link < <(daemon_pane "$agent" "$session")
    if [ -n "${pane:-}" ]; then
      export HERDR_PANE_ID="$pane"
      if [ "$link" = unlinked ] && [ -n "$session" ]; then
        transcript=""
        [ "$agent" = codex ] &&
          transcript="$(ls -t "$HOME"/.codex/sessions/*/*/*/rollout-*"$session".jsonl 2>/dev/null | head -1)"
        herdr pane report-agent-session "$pane" --source "herdr:$agent" --agent "$agent" \
          --seq "$(date +%s)000000000" --agent-session-id "$session" \
          ${transcript:+--agent-session-path "$transcript"} >/dev/null 2>&1
      fi
    fi
    ;;
esac

event="$(normalize)"
[ -z "${AGENT_EVENT_PRINT:-}" ] || { printf '%s\n' "$event"; exit 0; }
log_file="${XDG_STATE_HOME:-$HOME/.local/state}/agent-event.log"
printf '%s %s %s %s [%s]\n' "$(date '+%F %T')" "$agent" "$state" \
  "$(muxloc where 2>/dev/null || echo -)" "$*" >>"$log_file" 2>/dev/null

run() { # hook [args...]: in the background, the event on stdin
  local hook="$here/$1"
  shift
  [ -x "$hook" ] || return 0
  printf '%s' "$event" | nohup "$hook" "$@" >/dev/null 2>&1 &
}
for name in "$@"; do
  case "$name" in
    chime) run agent-chime.sh "$state" ;;
    toast) run agent-toast.sh ;;
    tab-rename) [ "$state" = "done" ] && run agent-tab-rename.sh ;;
    *) printf '%s unknown hook %s\n' "$(date '+%F %T')" "$name" >>"$log_file" 2>/dev/null ;;
  esac
done
exit 0
