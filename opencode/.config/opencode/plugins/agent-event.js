// agent-event.js -- opencode plugin: report each run to ~/.agents/hooks/agent-event.sh,
// the entry point the Claude, Codex and pi hooks use too, so opencode gets the
// same banner, chime and tab naming (the hooks it runs are listed in HOOKS).
//
//   done     when a turn ends (session.execution.succeeded, failed or
//            interrupted), with the session's last text and its title
//   blocked  on permission.asked, with what it wants to do
//
// Written for opencode v2's plugin format (a default export with an id and a
// setup function). v2 has no session.idle; a session's title arrives as
// session.renamed, just after its first turn ends, so the report waits a
// moment for it. opencode runs setup more than once (global and per project),
// so events are handled once across instances. Subagent sessions (those with
// a parent) are skipped. The script runs detached in the background, and a
// missing script or a failed spawn is ignored.
import { spawn } from "node:child_process";
import { homedir } from "node:os";
import { join } from "node:path";

const HOOK = join(homedir(), ".agents/hooks/agent-event.sh");
const HOOKS = ["chime", "toast", "tab-rename"];
const TITLE_WAIT_MS = 1500;
const ENDED = new Set(["session.execution.succeeded", "session.execution.failed", "session.execution.interrupted"]);

// Shared by every setup in this process, so a turn is reported once.
const shared = (globalThis.__agentEvent ??= { handled: new Set(), lastText: new Map(), titles: new Map() });

function send(state, payload) {
  try {
    const child = spawn(HOOK, ["opencode", state, ...HOOKS], {
      detached: true,
      stdio: ["pipe", "ignore", "ignore"],
    });
    child.on("error", () => {});
    child.stdin.on("error", () => {});
    child.stdin.end(JSON.stringify(payload));
    child.unref();
  } catch {
    // no hook script, or it can't start: opencode carries on without it
  }
}

// True the first time an event id is seen in this process.
function firstTime(id) {
  if (!id || shared.handled.has(id)) return !id;
  shared.handled.add(id);
  if (shared.handled.size > 1000) shared.handled.clear();
  return true;
}

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export default {
  id: "agent-event",
  setup(context) {
    const abort = new AbortController();

    const onEnded = async (sessionID) => {
      const reply = shared.lastText.get(sessionID) ?? "";
      shared.lastText.delete(sessionID);
      if (!shared.titles.has(sessionID)) await sleep(TITLE_WAIT_MS);
      let title = shared.titles.get(sessionID) ?? "";
      try {
        const got = await context.session.get({ sessionID });
        const session = got?.data ?? got;
        if (session?.parentID) return;
        title = title || session?.title || "";
      } catch {
        // can't read the session: still report that it finished
      }
      send("done", { session_id: sessionID, last_assistant_message: reply, title });
    };

    (async () => {
      try {
        for await (const event of context.event.subscribe({ signal: abort.signal })) {
          const data = event.data ?? {};
          if (event.type === "session.text.ended" && data.text) {
            shared.lastText.set(data.sessionID, data.text);
          } else if (event.type === "session.renamed" && data.title) {
            shared.titles.set(data.sessionID, data.title);
          } else if (ENDED.has(event.type) && firstTime(event.id)) {
            onEnded(data.sessionID);
          } else if (event.type === "permission.asked" && firstTime(event.id)) {
            const what = data.message || [data.action, ...(data.resources ?? [])].filter(Boolean).join(" ");
            send("blocked", { session_id: data.sessionID, message: what ? `wants ${what}` : "needs your permission" });
          }
        }
      } catch {
        // subscription ended (opencode shutting down or the plugin unloading)
      }
    })();

    return () => abort.abort();
  },
};
