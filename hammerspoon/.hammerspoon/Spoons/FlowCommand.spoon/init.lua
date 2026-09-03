--- === FlowCommand ===
---
--- Run something when a dictation starts with a name you registered.
---
--- Wispr Flow writes each finished transcription to a local SQLite
--- database. This Spoon polls it; a row opening with a registered name
--- ("hey jarvis ...") hands the rest of the sentence to that name's
--- action. Names are independent, so one can start a coding session and
--- another open a calendar.
---
--- Usage:
---     hs.loadSpoon("FlowCommand")
---     spoon.FlowCommand:addCommand({
---       name   = "jarvis",
---       action = spoon.FlowCommand.actions.tmux({ command = "claude" }),
---     })
---     spoon.FlowCommand:start()

local obj = {}
obj.__index = obj

obj.name    = "FlowCommand"
obj.version = "1.0"
obj.author  = "Khinshan Khan"
obj.license = "Apache-2.0"

obj.logger = hs.logger.new("FlowCommand", "info")

--- FlowCommand.dbPath (string)
--- Wispr Flow's transcription database. Opened read-only; it is
--- Wispr's file and a write would corrupt state it holds in memory.
obj.dbPath = os.getenv("HOME") ..
  "/Library/Application Support/Wispr Flow/flow.sqlite"

--- FlowCommand.greetings (table)
--- Words allowed in front of a name, crossed with every spelling by
--- addCommand.
obj.greetings = { "hey", "ok", "okay", "yo" }

--- FlowCommand.commands (table)
--- Registered names. Use addCommand rather than writing to this.
obj.commands = {}

--- FlowCommand.appFilter (string or nil)
--- Only accept transcripts whose app bundle id contains this. nil
--- accepts any.
---
--- Wispr types what it hears into the focused app and cannot be told
--- not to, so a spoken command leaves its text behind wherever you
--- were. Pointing this at FlowCommand.apps.scratchpad confines
--- commands to Wispr's own window.
obj.appFilter = nil

obj.apps = {
  scratchpad = "com.electron.wispr-flow",
}

--- FlowCommand.clearApps (table)
--- How to erase the dictated text once it has become a command:
--- "line" (ctrl-u, for shells), "field" (cmd-a delete, for a chat
--- composer), or absent to leave it.
---
--- Document editors are deliberately absent: cmd-a there selects the
--- file and the delete takes real work with it.
obj.clearApps = {
  ["com.mitchellh.ghostty"]     = "line",
  ["dev.warp.Warp-Stable"]      = "line",
  ["com.apple.Terminal"]        = "line",
  ["com.googlecode.iterm2"]     = "line",
  ["com.tinyspeck.slackmacgap"] = "field",
  ["com.electron.wispr-flow"]   = "field",
}

obj.clearStrategies = {
  line  = { { { "ctrl" }, "u" } },
  field = { { { "cmd" }, "a" }, { {}, "delete" } },
}

--- FlowCommand.watchInterval (number)
--- Seconds between stats of the database files.
obj.watchInterval = 1.0

--- FlowCommand.settleDelay (number)
--- Quiet period before reading. Wispr rewrites the row as it moves
--- from raw_transcript to formatted.
obj.settleDelay = 0.6

--- FlowCommand.maxAge (number)
--- Ignore transcripts older than this, so a stale row cannot fire on
--- reload.
obj.maxAge = 60

--- FlowCommand.pollInterval (number)
--- Seconds between checks during poll().
obj.pollInterval = 0.4

--- FlowCommand.pollTimeout (number)
--- How long poll() keeps looking.
obj.pollTimeout = 8

--- FlowCommand.tmuxBin (string)
--- Absolute path; a GUI process does not inherit the nix profile.
obj.tmuxBin = os.getenv("HOME") .. "/.nix-profile/bin/tmux"

--- FlowCommand.session (string)
--- Default tmux session for the tmux action.
obj.session = "flow"

--- FlowCommand.keeperWindow (string or nil)
--- Idle window that keeps the session alive. tmux destroys a session
--- when its last window exits, which would otherwise take the
--- scrollback of every finished command with it. Defaults to the
--- session's own name.
obj.keeperWindow = nil

--- FlowCommand.attach (string)
--- "switch" moves an attached terminal to the new window, "window"
--- opens another, "none" leaves it alone.
---
--- Prefer "switch": tmux sizes a window for the smallest client
--- viewing it, so a freshly opened terminal shrinks the pane.
obj.attach = "switch"

--- FlowCommand.workingDir (string or function)
--- Where commands run. A function is called at launch and returns the
--- path; see FlowCommand.dirs.
obj.workingDir = os.getenv("HOME")

obj.dirs = {}

--- FlowCommand.dirs.attachedOr(fallback) -> function
--- Function
--- The attached tmux client's current path, else fallback.
function obj.dirs.attachedOr(fallback)
  return function(self)
    local out, ok = hs.execute(string.format(
      "%s display-message -p '#{pane_current_path}' 2>/dev/null",
      self:_shellQuote(self.tmuxBin)))
    if ok and out then
      local path = out:gsub("%s+$", "")
      -- A detached server answers with a blank line rather than failing.
      if path ~= "" and hs.fs.attributes(path, "mode") == "directory" then
        return path
      end
    end
    return fallback
  end
end

obj.dirs.attached = obj.dirs.attachedOr(os.getenv("HOME"))

obj._lastId    = nil
obj._timer     = nil
obj._deadline  = 0
obj._watcher   = nil
obj._settle    = nil
obj._walPath   = nil
obj._lastMtime = nil
obj._hotkeys   = {}

-- Printable, because hs.execute does not deliver low control bytes
-- intact: a char(1) separator arrives stripped and every field parses
-- as nil.
local FIELD_SEP = "@@|@@"

--- FlowCommand:addCommand(spec) -> self
--- Method
--- Register a name and what it runs. Fields: name, action, and
--- optionally variants (other spellings the recogniser produces) and
--- label (shown in the alert).
---
--- Use a name of two syllables or more. A single letter does not
--- survive dictation: "hey C" came back as "Casey", and "C list the
--- files" as "CVList".
function obj:addCommand(spec)
  if type(spec) ~= "table" or not spec.name or not spec.action then
    error("FlowCommand:addCommand needs { name = ..., action = ... }", 2)
  end

  local spellings = { spec.name:lower() }
  for _, variant in ipairs(spec.variants or {}) do
    table.insert(spellings, variant:lower())
  end

  local phrases = {}
  for _, spelling in ipairs(spellings) do
    table.insert(phrases, spelling)
    for _, greeting in ipairs(self.greetings) do
      table.insert(phrases, greeting .. " " .. spelling)
    end
  end

  table.insert(self.commands, {
    name    = spec.name,
    label   = spec.label or spec.name,
    action  = spec.action,
    phrases = phrases,
  })
  return self
end

--- FlowCommand:clearCommands() -> self
--- Method
--- Forget every command, so a reloaded config re-registers rather than
--- duplicating.
function obj:clearCommands()
  self.commands = {}
  return self
end

--- FlowCommand:match(text) -> command, prompt
--- Method
--- The command whose name `text` opens with, and the rest of the text.
--- The longest phrase across all commands wins, so "hey jarvis" beats
--- "jarvis" and a name is never shadowed by another that prefixes it.
function obj:match(text)
  local lower = text:lower()
  local best, bestCommand

  for _, command in ipairs(self.commands) do
    for _, phrase in ipairs(command.phrases) do
      if lower:sub(1, #phrase) == phrase
        and (not best or #phrase > #best) then
        -- Word boundary, so "jarvisonian" does not match "jarvis".
        local following = text:sub(#phrase + 1, #phrase + 1)
        if following == "" or following:match("[%s,%.!%?:;]") then
          best, bestCommand = phrase, command
        end
      end
    end
  end

  if not best then return nil end

  local rest = text:sub(#best + 1):gsub("^[%s,%.!%?:;]+", "")
  if rest == "" then return nil end
  return bestCommand, rest
end

--- FlowCommand:stripWake(text) -> string or nil
--- Method
--- The text after the name, ignoring which command matched.
function obj:stripWake(text)
  local _, prompt = self:match(text)
  return prompt
end

--- FlowCommand.actions (table)
--- Factories for addCommand's `action`.
obj.actions = {}

--- FlowCommand.actions.tmux(opts) -> function
--- Function
--- Run a program in a new tmux window with the dictation as its last
--- argument. opts: command (required), args, session, keeper, dir,
--- attach. Anything omitted falls back to the Spoon-level setting.
function obj.actions.tmux(opts)
  opts = opts or {}
  if not opts.command then
    error("FlowCommand.actions.tmux needs { command = ... }", 2)
  end
  return function(prompt, self)
    self:_launchTmux(prompt, opts)
  end
end

--- FlowCommand.actions.shell(opts) -> function
--- Function
--- Run a program in the background. The dictation is $FLOW_PROMPT, and
--- is appended as an argument unless `args` already mentions it.
function obj.actions.shell(opts)
  opts = opts or {}
  if not opts.command then
    error("FlowCommand.actions.shell needs { command = ... }", 2)
  end
  return function(prompt, self)
    local q = function(s) return self:_shellQuote(s) end
    local line = q(opts.command) .. " " .. (opts.args or "")
    if not (opts.args or ""):find("FLOW_PROMPT", 1, true) then
      line = line .. ' "$FLOW_PROMPT"'
    end
    self:_run("export FLOW_PROMPT=" .. q(prompt) .. "\n" ..
      line .. " >/dev/null 2>&1 &", "shell action")
  end
end

--- FlowCommand.actions.url(template) -> function
--- Function
--- Open a URL, replacing ${prompt} with the encoded dictation.
function obj.actions.url(template)
  return function(prompt, self)
    local url = template:gsub("%${prompt}", function()
      return hs.http.encodeForQuery(prompt)
    end)
    hs.urlevent.openURL(url)
  end
end

--- FlowCommand:resolveDir([override]) -> string
--- Method
function obj:resolveDir(override)
  local dir = override or self.workingDir
  if type(dir) == "function" then return dir(self) end
  return dir
end

--- FlowCommand:windowName(prompt) -> string
--- Method
function obj:windowName(prompt)
  local name = prompt:sub(1, 24):gsub("[^%w%s-]", ""):gsub("%s+", "-")
  name = name:gsub("^-+", ""):gsub("-+$", "")
  return name ~= "" and name or "cmd"
end

--- FlowCommand:_shellQuote(s) -> string
--- Method
--- Single-quote for a POSIX shell. Lua's %q emits backslash escapes
--- that a shell reads literally, so it cannot be used for this.
function obj:_shellQuote(s)
  return "'" .. tostring(s):gsub("'", "'\\''") .. "'"
end

function obj:_launchTmux(prompt, opts)
  opts = opts or {}
  local shell   = os.getenv("SHELL") or "/bin/zsh"
  local command = opts.command
  local args    = opts.args or ""
  local session = opts.session or self.session
  local keeper  = opts.keeper  or self.keeperWindow or session
  local attach  = opts.attach  or self.attach

  local q = function(s) return self:_shellQuote(s) end
  local dir = self:resolveDir(opts.dir)

  -- The prompt travels as FLOW_PROMPT because tmux re-parses a
  -- new-window command through a shell: quoting it inline would have
  -- to survive Lua, this script, and tmux in turn, and the window dies
  -- silently when it does not.
  local inner = string.format('exec %s %s "$FLOW_PROMPT"', q(command), args)

  local script = table.concat({
    "set -e",
    "TMUX_BIN=" .. q(self.tmuxBin),
    "export FLOW_PROMPT=" .. q(prompt),
    '"$TMUX_BIN" has-session -t ' .. q(session) .. " 2>/dev/null ||",
    '  "$TMUX_BIN" new-session -d -s ' .. q(session) ..
      " -n " .. q(keeper) .. " -c " .. q(dir),
    -- `session:` with the colon, since bare `-t session` means window 0
    -- and tmux refuses with "index 0 in use". Shell, flag and command
    -- stay separate words so tmux takes them as argv.
    '"$TMUX_BIN" new-window -t ' .. q(session .. ":") ..
      " -n " .. q(self:windowName(prompt)) ..
      " -c " .. q(dir) ..
      ' -e FLOW_PROMPT="$FLOW_PROMPT" ' ..
      q(shell) .. " -lc " .. q(inner),
  }, "\n")

  local openTerminal = "/usr/bin/open -na Ghostty.app --args -e " ..
    q(shell) .. " -lc " ..
    q(string.format("exec %s attach-session -t %s", q(self.tmuxBin), q(session)))

  if attach == "switch" then
    script = script .. "\n" .. table.concat({
      'if [ -n "$("$TMUX_BIN" list-clients -F ok 2>/dev/null)" ]; then',
      '  "$TMUX_BIN" switch-client -t ' .. q(session .. ":") ..
        " >/dev/null 2>&1 || true",
      "  /usr/bin/open -a Ghostty.app",
      "else",
      "  " .. openTerminal,
      "fi",
    }, "\n")
  elseif attach == "window" then
    script = script .. "\n" .. openTerminal
  end

  self:_run(script, "launch")
  self.logger:i("launched: " .. prompt)
end

-- Written to a file and run by path: hs.execute passes its argument as
-- a single -c and returns nil having run nothing when given several
-- lines. hs.task would be the obvious alternative but creating one
-- crashes this Hammerspoon build.
function obj:_run(script, label)
  local path = os.tmpname()
  local fh = io.open(path, "w")
  if not fh then
    self.logger:e(label .. ": could not write script")
    return false
  end
  fh:write("#!/bin/zsh\n" .. script .. "\n")
  fh:close()

  local out, ok = hs.execute(
    string.format("/bin/zsh %s 2>&1", self:_shellQuote(path)), true)
  os.remove(path)

  if not ok then
    self.logger:e(string.format("%s failed: %s", label, tostring(out)))
    hs.alert.show("Flow: " .. label .. " failed", 2)
    return false
  end
  return true
end

function obj:_query()
  local uri = "file:" .. self.dbPath .. "?mode=ro"
  local sep = "'" .. FIELD_SEP .. "'"
  local sql =
    "SELECT transcriptEntityId || " .. sep .. " || " ..
    "CAST((julianday('now') - julianday(timestamp)) * 86400.0 AS INTEGER) || " .. sep .. " || " ..
    "COALESCE(app, '') || " .. sep .. " || " ..
    "REPLACE(COALESCE(formattedText, asrText, ''), char(10), ' ') " ..
    "FROM History WHERE status = 'formatted' " ..
    "ORDER BY timestamp DESC LIMIT 1;"

  local out, ok = hs.execute(string.format(
    [[/usr/bin/sqlite3 -readonly -noheader -list %q %q 2>/dev/null]],
    uri, sql))
  if not ok or not out or out == "" then return nil end

  local pattern = FIELD_SEP:gsub("(%W)", "%%%1")
  local id, age, app, text = out:match(
    "^(.-)" .. pattern .. "(.-)" .. pattern .. "(.-)" .. pattern .. "(.-)%s*$")
  if not id or id == "" then return nil end

  return { id = id, age = tonumber(age) or 0, app = app, text = text }
end

function obj:_mtime()
  -- Both: the WAL takes commits, a checkpoint folds them into the db.
  local wal = hs.fs.attributes(self._walPath, "modification") or 0
  local db  = hs.fs.attributes(self.dbPath, "modification") or 0
  return wal .. ":" .. db
end

--- FlowCommand:clearPastedText() -> boolean
--- Method
--- Erase what Wispr typed into the focused app, if clearApps says how.
function obj:clearPastedText()
  local app = hs.application.frontmostApplication()
  if not app then return false end

  local bundle = app:bundleID()
  local strategy = bundle and self.clearApps[bundle]
  if not strategy then return false end

  local keys = self.clearStrategies[strategy]
  if not keys then
    self.logger:w("unknown clear strategy '" .. tostring(strategy) .. "'")
    return false
  end

  -- Aimed at the application, so a focus change between firing and now
  -- cannot send the keystroke somewhere unintended.
  for _, stroke in ipairs(keys) do
    hs.eventtap.keyStroke(stroke[1], stroke[2], 0, app)
  end
  return true
end

--- FlowCommand:check() -> boolean
--- Method
--- Look at the newest transcript once; true when it fired a command.
function obj:check()
  local row = self:_query()
  if not row then return false end

  if row.id == self._lastId then return false end
  self._lastId = row.id

  if row.age > self.maxAge then return false end

  if self.appFilter and not row.app:find(self.appFilter, 1, true) then
    return false
  end

  local command, prompt = self:match(row.text)
  if not command then return false end

  -- Before the action, while the app Wispr typed into is still front.
  self:clearPastedText()

  hs.alert.closeAll()
  hs.alert.show(string.format("🤖  %s: %s", command.label, prompt), 1.2)

  local ok, err = pcall(command.action, prompt, self)
  if not ok then
    self.logger:e(string.format("command '%s' failed: %s",
      command.name, tostring(err)))
    hs.alert.show("Flow: " .. command.label .. " failed", 2)
  end
  return true
end

--- FlowCommand:poll()
--- Method
--- Watch for the next transcript until one fires or pollTimeout passes.
function obj:poll()
  -- Adopt the current row as seen, so only what is dictated next fires.
  local row = self:_query()
  if row then self._lastId = row.id end

  self._deadline = hs.timer.secondsSinceEpoch() + self.pollTimeout
  if self._timer then self._timer:stop() end

  self._timer = hs.timer.doEvery(self.pollInterval, function()
    if self:check() or hs.timer.secondsSinceEpoch() > self._deadline then
      self._timer:stop()
      self._timer = nil
    end
  end)
  return self
end

--- FlowCommand:start()
--- Method
--- Begin watching. Existing rows are marked seen first.
---
--- Polls mtimes rather than using hs.pathwatcher: SQLite writes the WAL
--- through a memory mapping and FSEvents does not report those, so the
--- watcher saw Wispr's caches change all day and never a transcript.
function obj:start()
  local row = self:_query()
  if row then self._lastId = row.id end

  self._walPath = self.dbPath .. "-wal"
  self._lastMtime = self:_mtime()

  self._watcher = hs.timer.doEvery(self.watchInterval, function()
    local mtime = self:_mtime()
    if mtime == self._lastMtime then return end
    self._lastMtime = mtime

    if self._settle then self._settle:stop() end
    self._settle = hs.timer.doAfter(self.settleDelay, function()
      self._settle = nil
      self:check()
    end)
  end)

  self.logger:i("started; polling " .. self._walPath)
  return self
end

--- FlowCommand:stop()
--- Method
function obj:stop()
  if self._watcher then self._watcher:stop(); self._watcher = nil end
  if self._settle then self._settle:stop(); self._settle = nil end
  if self._timer then self._timer:stop(); self._timer = nil end
  return self
end

--- FlowCommand:bindHotkeys(mapping)
--- Method
--- Actions: arm (watch for the next dictation), check (try the newest
--- transcript now).
function obj:bindHotkeys(mapping)
  for _, hotkey in ipairs(self._hotkeys) do hotkey:delete() end
  self._hotkeys = {}

  local actions = {
    arm   = function()
      hs.alert.show("Listening for a command", 0.8)
      self:poll()
    end,
    check = function()
      if not self:check() then
        hs.alert.show("No command in the last transcript", 1)
      end
    end,
  }

  for name, spec in pairs(mapping or {}) do
    local action = actions[name]
    if action then
      table.insert(self._hotkeys, hs.hotkey.bind(spec[1], spec[2], action))
    else
      self.logger:w("bindHotkeys: unknown action '" .. tostring(name) .. "'")
    end
  end
  return self
end

return obj
