--- === DjiWispr ===
---
--- Toggle Wispr Flow hands-free dictation with the DJI Mic Mini button.
---
--- The mic's button, via the USB receiver ("Wireless Mic Rx"), emits
--- key=SOUND_UP. That is indistinguishable from the Mac keyboard's
--- volume-up key (identical eventSourceStateID / PID / keyboardType),
--- so the two cannot be told apart. This Spoon therefore only
--- intercepts the trigger key while the receiver is the default INPUT
--- device; the rest of the time volume-up behaves normally.
---
--- Usage:
---     hs.loadSpoon("DjiWispr")
---     spoon.DjiWispr.micMatch = "Wireless Mic Rx"
---     spoon.DjiWispr:start():bindHotkeys({
---       toggle = {{"ctrl","alt","cmd"}, "D"},
---     })

local obj = {}
obj.__index = obj

obj.name    = "DjiWispr"
obj.version = "1.0"
obj.author  = "Khinshan Khan"
obj.license = "Apache-2.0"

--- DjiWispr.micMatch (string)
--- Substring identifying the receiver among input devices. The button
--- is only intercepted while this device is the default input.
obj.micMatch = "Wireless Mic Rx"

--- DjiWispr.triggerKey (string)
--- Which system media key the mic button emits. Ours sends "SOUND_UP";
--- other units/firmware may send "SOUND_DOWN", "PLAY", "NEXT",
--- "PREVIOUS" or "MUTE".
obj.triggerKey = "SOUND_UP"

--- DjiWispr.debounce (number)
--- Seconds to ignore repeat trigger events after a toggle.
obj.debounce = 0.35

--- DjiWispr.volumeStep (number)
--- Percent change per press of the volumeUp/volumeDown hotkeys.
obj.volumeStep = 6

--- DjiWispr.watchdogInterval (number)
--- How often to check that macOS has not silently disabled the tap.
obj.watchdogInterval = 10

obj.logger = hs.logger.new("DjiWispr", "info")

obj._dictating = false
obj._suspended = false
obj._lastFire  = 0
obj._tap       = nil
obj._watchdog  = nil
obj._hotkeys   = {}

function obj:_micIsDefaultInput()
  local dev = hs.audiodevice.defaultInputDevice()
  return dev ~= nil and (dev:name() or ""):find(self.micMatch, 1, true) ~= nil
end

function obj:_nudgeVolume(delta)
  local dev = hs.audiodevice.defaultOutputDevice()
  if not dev then return end
  local level = (dev:volume() or 0) + delta
  dev:setVolume(math.max(0, math.min(100, level)))
end

function obj:_handleEvent(event)
  local key = event:systemKey()
  if not (key and key.key == self.triggerKey) then return false end

  if self._suspended or not self:_micIsDefaultInput() then return false end

  -- Ours from here on, so swallow the key-up half too.
  if not key.down then return true end

  local now = hs.timer.secondsSinceEpoch()
  if (now - self._lastFire) < self.debounce then return true end
  self._lastFire = now

  self:toggle()
  return true
end

function obj:_checkTap()
  if self._tap and not self._tap:isEnabled() then
    self.logger.w("tap was disabled by the system - restarting")
    self._tap:start()
  end
end

--- DjiWispr:setDictating(on)
--- Method
--- Start or stop Wispr Flow hands-free dictation.
function obj:setDictating(on)
  self._dictating = on
  hs.execute(string.format('open "wispr-flow://%s-hands-free"', on and "start" or "stop"))
  hs.alert.closeAll()
  hs.alert.show(on and "🎙  Dictation ON" or "⏹  Dictation OFF", 0.7)
  self.logger.i("dictating -> " .. tostring(on))
  return self
end

--- DjiWispr:toggle()
--- Method
--- Flip dictation on/off.
function obj:toggle()
  return self:setDictating(not self._dictating)
end

--- DjiWispr:setSuspended(state)
--- Method
--- Suspend or resume interception of the mic button. While suspended
--- the trigger key behaves normally.
function obj:setSuspended(state)
  self._suspended = state
  hs.alert.show(state and "Mic button: SUSPENDED" or "Mic button: active", 1)
  self.logger.i("suspended -> " .. tostring(state))
  return self
end

--- DjiWispr:isDictating() -> boolean
--- Method
--- Whether dictation is currently believed to be on.
function obj:isDictating()
  return self._dictating
end

--- DjiWispr:init()
--- Method
--- Called automatically by hs.loadSpoon().
function obj:init()
  self._tap = hs.eventtap.new(
    { hs.eventtap.event.types.systemDefined },
    function(event) return self:_handleEvent(event) end
  )
  return self
end

--- DjiWispr:start()
--- Method
--- Begin intercepting the mic button.
function obj:start()
  if not self._tap then self:init() end
  self._tap:start()

  self._watchdog = hs.timer.new(self.watchdogInterval, function() self:_checkTap() end)
  self._watchdog:start()

  -- NOTE: hs.audiodevice.watcher is a process-wide singleton, so this
  -- replaces any callback another module set. Acceptable while this is
  -- the only consumer; revisit if that changes.
  hs.audiodevice.watcher.setCallback(function()
    if self._dictating and not self:_micIsDefaultInput() then
      self:setDictating(false)
    end
  end)
  hs.audiodevice.watcher.start()

  self.logger.i("started; tap=" .. tostring(self._tap:isEnabled()))
  return self
end

--- DjiWispr:stop()
--- Method
--- Stop intercepting; the trigger key returns to normal everywhere.
function obj:stop()
  if self._tap then self._tap:stop() end
  if self._watchdog then
    self._watchdog:stop()
    self._watchdog = nil
  end
  hs.audiodevice.watcher.stop()
  if self._dictating then self:setDictating(false) end
  self.logger.i("stopped")
  return self
end

--- DjiWispr:bindHotkeys(mapping)
--- Method
--- Recognised actions: toggle, suspend, volumeUp, volumeDown.
--- Unbound actions are simply skipped.
function obj:bindHotkeys(mapping)
  for _, hotkey in ipairs(self._hotkeys) do hotkey:delete() end
  self._hotkeys = {}

  local actions = {
    toggle     = function() self:toggle() end,
    suspend    = function() self:setSuspended(not self._suspended) end,
    volumeUp   = function() self:_nudgeVolume(self.volumeStep) end,
    volumeDown = function() self:_nudgeVolume(-self.volumeStep) end,
  }

  for name, spec in pairs(mapping or {}) do
    local action = actions[name]
    if action then
      table.insert(self._hotkeys, hs.hotkey.bind(spec[1], spec[2], action))
    else
      self.logger.w("bindHotkeys: unknown action '" .. tostring(name) .. "'")
    end
  end
  return self
end

return obj
