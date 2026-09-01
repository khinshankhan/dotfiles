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

hs.alert.show("Hammerspoon loaded", 1)
