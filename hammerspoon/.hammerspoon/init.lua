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

spoon.FlowCommand.session = "flow"
spoon.FlowCommand.attach = "switch"
spoon.FlowCommand.workingDir = spoon.FlowCommand.dirs.attachedOr(
  os.getenv("HOME") .. "/development/lab")

-- Cleared first so a reload re-registers rather than duplicating.
spoon.FlowCommand:clearCommands()

spoon.FlowCommand:addCommand({
  name     = "jarvis",
  variants = { "jarvus", "javis", "jervis" },
  action   = spoon.FlowCommand.actions.tmux({
    command = os.getenv("HOME") .. "/.local/bin/claude",
  }),
})

-- More names go here. Each tmux action can pin its own directory and
-- session, or take the defaults above:
--
--   spoon.FlowCommand:addCommand({
--     name   = "dotty",
--     action = spoon.FlowCommand.actions.tmux({
--       command = os.getenv("HOME") .. "/.local/bin/claude",
--       dir     = os.getenv("HOME") .. "/dotfiles",
--       session = "dots",
--     }),
--   })
--
--   spoon.FlowCommand:addCommand({
--     name   = "mavis",
--     action = spoon.FlowCommand.actions.url("x-apple-calendar://"),
--   })

spoon.FlowCommand:start()
spoon.FlowCommand:bindHotkeys({
  arm   = {{"ctrl","alt","cmd"}, "J"},
  check = {{"ctrl","alt","cmd","shift"}, "J"},
})

hs.alert.show("Hammerspoon loaded", 1)
