// agent-event.ts -- pi extension: report each run to ~/.agents/hooks/agent-event.sh,
// the entry point the Claude and Codex hooks use too, so pi gets the same
// banner and chime (the hooks it runs are listed in HOOKS).
//
//   done     on agent_settled, when pi won't continue on its own (after any
//            retries or compaction), with the last reply and the prompts seen
//   blocked  on ui_prompt_start, while pi waits on a blocking prompt
//
// The script runs detached in the background, so pi never waits on it, and a
// missing script or a failed spawn is ignored.
import { spawn } from "node:child_process";
import { homedir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const HOOK = join(homedir(), ".agents/hooks/agent-event.sh");
// Which of agent-event.sh's hooks pi runs. No tab-rename: pi tabs keep the
// name you give them.
const HOOKS = ["chime", "toast"];
const MAX_PROMPTS = 20;

// A message's text: a plain string, or its text blocks joined.
function text(content: unknown): string {
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  return content
    .filter((block): block is { type: "text"; text: string } => block?.type === "text")
    .map((block) => block.text)
    .join("\n");
}

function send(state: "done" | "blocked", payload: Record<string, unknown>): void {
  try {
    const child = spawn(HOOK, ["pi", state, ...HOOKS], { detached: true, stdio: ["pipe", "ignore", "ignore"] });
    child.on("error", () => {});
    child.stdin.on("error", () => {});
    child.stdin.end(JSON.stringify(payload));
    child.unref();
  } catch {
    // no hook script, or it can't start: pi carries on without it
  }
}

export default function (pi: ExtensionAPI): void {
  let prompts: string[] = [];
  let lastReply = "";

  pi.on("session_start", () => {
    prompts = [];
    lastReply = "";
  });

  pi.on("agent_end", (event) => {
    for (const message of event.messages) {
      const t = text((message as { content?: unknown }).content);
      if (!t) continue;
      if (message.role === "user" && prompts.length < MAX_PROMPTS) prompts.push(t);
      if (message.role === "assistant") lastReply = t;
    }
  });

  pi.on("agent_settled", () => {
    send("done", { last_assistant_message: lastReply, prompts });
  });

  pi.on("ui_prompt_start", (event) => {
    send("blocked", { message: event.title ?? `waiting on a ${event.kind}` });
  });
}
