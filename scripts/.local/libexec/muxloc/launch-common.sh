#!/usr/bin/env bash
# Launch a command in tmux or Herdr, from a terminal or a GUI caller.
#
# Usage: muxloc launch --backend <tmux|herdr> --command EXE [options] [-- args...]
#        t launch --command EXE [options] [-- args...]
#        herdrctl launch --command EXE [options] [-- args...]
#
# Options:
#   --backend MUX        tmux or herdr (required for muxloc launch)
#   --command EXE        executable name or path (required)
#   --dir PATH           run in this directory
#   --fallback-dir PATH  use the focused pane's directory, else PATH (default: PWD)
#   --workspace NAME     reuse or create a workspace/session (tmux default: flow;
#                        Herdr default: current; an existing Herdr ID also works)
#   --tab NAME           optional tab/window name (default: automatic naming)
#   --attach MODE        switch, window, or none (default: switch)
#   --keeper NAME        tmux idle window (default: session name)
#   -h, --help, help     this text
#
# Arguments after -- are passed unchanged, never interpreted as shell code.
# For shell syntax, explicitly launch a shell, eg --command sh -- -c '...'.
# Terminal activation uses Ghostty on macOS; use --attach none on other
# systems, with Herdr already running if selected.
#
# Examples:
#   muxloc launch --backend tmux --command npm -- run dev
#   muxloc launch --backend herdr --command claude --workspace flow \
#     -- --model sonnet -- 'Review the diff'

# Sourced by launch commands; parsed options are shared with their callers.
# shellcheck shell=bash disable=SC2034

help() { sed -n '2,/^$/{/^$/d; s/^# \{0,1\}//; p;}' "${BASH_SOURCE[0]}"; }
die() { printf 'muxloc launch: %s\n' "$*" >&2; exit 1; }
usage_error() { printf 'muxloc launch: %s (see muxloc launch --help)\n' "$*" >&2; exit 2; }
quote() {
  local escaped_quote="'\\''"
  printf "'%s'" "${1//\'/$escaped_quote}"
}

parse_launch_options() {
  launch_command="" dir="" fallback_dir="$PWD"
  label="" attach=switch keeper="" workspace=""
  while [ "$#" -gt 0 ]; do
    case "$1" in
      -h | --help | help) help; exit 0 ;;
      --) shift; break ;;
      --backend | --command | --dir | --fallback-dir | --tab | --attach | --keeper | --workspace)
        [ "$#" -ge 2 ] && [ -n "$2" ] || usage_error "$1 needs a value"
        case "$1" in
          --backend) backend="$2" ;;
          --command) launch_command="$2" ;;
          --dir) dir="$2" ;;
          --fallback-dir) fallback_dir="$2" ;;
          --tab) label="$2" ;;
          --attach) attach="$2" ;;
          --keeper) keeper="$2" ;;
          --workspace) workspace="$2" ;;
        esac
        shift 2 ;;
      *) usage_error "unexpected argument: $1" ;;
    esac
  done
  command_args=("$@")
  [ -n "$launch_command" ] || usage_error "--command is required"
  case "$attach" in switch | window | none) ;; *) usage_error "invalid attach mode: $attach" ;; esac
  case "$backend" in tmux | herdr) ;; *) usage_error "--backend must be tmux or herdr" ;; esac
  if [ "$attach" != none ] && [ "$(uname -s)" != Darwin ]; then
    usage_error "terminal activation requires macOS; use --attach none"
  fi

  # An explicit directory must exist; only discovered paths use the fallback.
  if [ -n "$dir" ] && [ ! -d "$dir" ]; then die "directory does not exist: $dir"; fi
}

resolve_dir() {
  [ -n "$dir" ] && [ -d "$dir" ] || dir="$fallback_dir"
  [ -d "$dir" ] || die "directory does not exist: $dir"
}
resolve_command() {
  command_bin="$(type -P "$launch_command")" ||
    die "executable not found: $launch_command"
  # Keep relative executable paths anchored to the caller's directory.
  case "$command_bin" in /*) ;; *) command_bin="$PWD/$command_bin" ;; esac
}
launch_shell="${SHELL:-/bin/zsh}"
open_terminal() { /usr/bin/open -na Ghostty.app --args -e "$launch_shell" -lc "$1"; }
focus_terminal() { /usr/bin/open -a Ghostty.app; }
