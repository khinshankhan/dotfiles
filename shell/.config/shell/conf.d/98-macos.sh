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

# Memory helpers use subshells to keep shell options and traps local.
am() (
    set -euo pipefail

    if [ "$#" -gt 0 ]; then
        if [ "$#" -eq 1 ]; then
            case "$1" in
                help | -h | --help)
                    cat <<'EOF'
am -- show the processes using the most memory on macOS.

Usage: am             show up to 10 processes, sorted by memory
       am help        this text (also -h, --help)

Uses macOS top. Columns: NAME, PID, MEM_GB, THREADS, PORTS, USER.
Memory is shown in GB. Run without arguments for a single snapshot.
EOF
                    exit 0
                    ;;
            esac
        fi
        echo "am: unexpected arguments (see am --help)" >&2
        exit 2
    fi

    printf "%-30s %-8s %-8s %-8s %-8s %-15s\n" "NAME" "PID" "MEM_GB" "THREADS" "PORTS" "USER"
    top -l 1 -o mem |
      awk 'BEGIN{start=0}
    /^PID[[:space:]]+COMMAND/{start=1; next}
    start && NF>0 {
      name=$2; pid=$1; th=$5; ports=$7; mem=$8; user=$NF;
      if (mem ~ /G$/) gb=substr(mem,1,length(mem)-1)+0;
      else if (mem ~ /M$/) gb=(substr(mem,1,length(mem)-1)+0)/1024;
      else if (mem ~ /K$/) gb=(substr(mem,1,length(mem)-1)+0)/1024/1024;
      else next;
      printf "%-30s %-8s %-8.2f %-8s %-8s %-15s\n", name, pid, gb, th, ports, user
    }' | head -10
)

appmem() (
    set +e            # footprint can fail on processes we cannot inspect
    set -uo pipefail

    if [ "$#" -gt 0 ]; then
        if [ "$#" -eq 1 ]; then
            case "$1" in
                help | -h | --help)
                    cat <<'EOF'
appmem -- show memory grouped by app or process tree on macOS.

Usage: appmem             show up to 15 groups, sorted by memory
       appmem help        this text (also -h, --help)

Groups app helpers by their outermost .app bundle. Other processes group
under their highest non-launchd ancestor and are labeled "(tree)".

Sums phys_footprint from macOS footprint, not RSS, and prints totals in GB.
Only includes processes footprint can inspect without root. Run without
arguments for a single snapshot; inspecting each process can take a while.
EOF
                    exit 0
                    ;;
            esac
        fi
        echo "appmem: unexpected arguments (see appmem --help)" >&2
        exit 2
    fi

    tmp=$(mktemp)
    trap 'rm -f "$tmp"' EXIT

    # Map each PID to a group. Apps group by their outermost .app bundle; everything
    # else groups under the name of its top-level ancestor, which is what catches
    # a tmux server full of shells (the Force Quit panel credits those to the
    # terminal via a private "responsible process" API this cannot see).
    ps -eo pid=,ppid=,command= | awk '
    {
        pid = $1; ppid[$1] = $2
        cmd = substr($0, index($0, $3))
        line[pid] = cmd
        n = split(cmd, w, " ")
        split(w[1], path, "/")
        name[pid] = path[length(path)]
    }
    END {
        for (p in line) {
            i = index(line[p], ".app/")
            if (i > 0) {
                head = substr(line[p], 1, i - 1)
                n = split(head, parts, "/")
                if (parts[n] != "") { print p "\t" parts[n]; continue }
            }
            # Walk to the highest non-launchd ancestor and group under its name.
            cur = p
            while (ppid[cur] != "" && ppid[cur] > 1 && cur != ppid[cur]) cur = ppid[cur]
            if (cur != p || ppid[p] <= 1) print p "\t" name[cur] " (tree)"
            else print p "\t" name[cur] " (tree)"
        }
    }' > "$tmp"

    # One footprint(1) per pid, so this takes a second or two.
    while IFS=$'\t' read -r pid app; do
        footprint -p "$pid" 2>/dev/null | awk -v app="$app" '
            /phys_footprint:/ {
                v = $2
                if ($3 == "GB")      v *= 1024
                else if ($3 == "KB") v /= 1024
                print v "\t" app
            }'
    done < "$tmp" | awk -F'\t' '
        { total[$2] += $1 }
        END { for (a in total) printf "%9.2f GB  %s\n", total[a]/1024, a }
    ' | sort -rn | head -15
)
