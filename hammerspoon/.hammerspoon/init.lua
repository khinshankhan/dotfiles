-- Initialize hammerspoon
-- open -a Hammerspoon

require("hs.ipc")

hs.loadSpoon("DjiWispr")
spoon.DjiWispr:start()
spoon.DjiWispr:bindHotkeys({
  toggle   = {{"ctrl","alt","cmd"}, "D"},
  suspend  = {{"ctrl","alt","cmd"}, "V"},
  volumeUp = {{"ctrl","alt","cmd"}, "="},  -- SOUND_UP is swallowed while the mic is active
})

hs.loadSpoon("FlowCommand")

-- Choose "tmux" or "herdr" for voice-launched commands.
local flowBackend = "herdr"

-- Cleared first so a reload re-registers rather than duplicating.
spoon.FlowCommand:clearCommands()

spoon.FlowCommand:addCommand({
  name     = "jarvis",
  variants = { "jarvus", "javis", "jervis" },
  -- The dictation is appended as one final argument.
  command  = {
    os.getenv("HOME") .. "/.local/bin/muxloc", "launch",
    "--backend", flowBackend,
    "--workspace", "flow",
    "--fallback-dir", os.getenv("HOME") .. "/development/lab",
    "--attach", "switch",
    "--command", "claude",
    "--", "--",
  },
})

spoon.FlowCommand:start()
spoon.FlowCommand:bindHotkeys({
  arm   = {{"ctrl","alt","cmd"}, "J"},
  check = {{"ctrl","alt","cmd","shift"}, "J"},
})

hs.alert.show("Hammerspoon loaded", 1)
