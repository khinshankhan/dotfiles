-- Rex configuration. Bind keys with rex.bind, remove defaults with rex.unbind.
-- Check with `rex config check`, apply with `rex config reload`.
--
-- Rex for Mac's default shortcuts, written out explicitly so they are visible
-- and editable here. Source: the Mac Actions reference in the Rex docs.
--
-- Not listed: the vim-style copy mode keys (h/j/k/l, v, y, ...) and the
-- pane.send_key line-editing keys (cmd+left/right/backspace, alt+left/right).
-- Those only make sense inside the app, and the client keeps its built-in
-- behavior for them.
--
-- Also not listed: cmd+1 through cmd+9 (client.tab.goto). Binding them here
-- replaces the app's own and hides the number hints shown while holding cmd.

-- Window
rex.bind("cmd+n", "client.window.new")
rex.bind("shift+cmd+w", "client.window.close")
rex.bind("ctrl+cmd+f", "client.window.fullscreen.toggle")
rex.bind("cmd+w", "client.close")

-- App
rex.bind("shift+cmd+p", "client.palette.toggle")
rex.bind("cmd+,", "client.settings")
rex.bind("shift+cmd+,", "client.diagnostics")
rex.bind("shift+cmd+u", "client.update.accept")
rex.bind("alt+shift+cmd+u", "client.update.accept_when_idle")
rex.bind("shift+cmd+.", "client.notice.dismiss")

-- Session
rex.bind("shift+cmd+n", "session.new")
rex.bind("alt+shift+cmd+n", "session.new.on_host")
rex.bind("cmd+k", "session.switch")
rex.bind("cmd+r", "session.refresh")

-- Tab
rex.bind("cmd+t", "client.tab.new")
rex.bind("alt+cmd+w", "client.tab.close")
rex.bind("ctrl+tab", "client.tab.next")
rex.bind("shift+cmd+]", "client.tab.next")
rex.bind("ctrl+shift+tab", "client.tab.previous")
rex.bind("shift+cmd+[", "client.tab.previous")
rex.bind("ctrl+shift+pageup", "client.tab.move.backward")
rex.bind("ctrl+shift+pagedown", "client.tab.move.forward")
rex.bind("alt+cmd+u", "client.tab.unread")

-- Pane
rex.bind("shift+cmd+left", "pane.split.left")
rex.bind("cmd+d", "pane.split.right")
rex.bind("shift+cmd+right", "pane.split.right")
rex.bind("shift+cmd+up", "pane.split.up")
rex.bind("shift+cmd+d", "pane.split.down")
rex.bind("shift+cmd+down", "pane.split.down")
rex.bind("cmd+enter", "pane.split.auto")
rex.bind("alt+cmd+left", "pane.focus.left")
rex.bind("alt+cmd+right", "pane.focus.right")
rex.bind("alt+cmd+up", "pane.focus.up")
rex.bind("alt+cmd+down", "pane.focus.down")
rex.bind("cmd+]", "pane.focus_next")
rex.bind("cmd+[", "pane.focus_previous")
rex.bind("alt+shift+cmd+left", "pane.move.left")
rex.bind("alt+shift+cmd+right", "pane.move.right")
rex.bind("alt+shift+cmd+up", "pane.move.up")
rex.bind("alt+shift+cmd+down", "pane.move.down")
rex.bind("alt+cmd+enter", "pane.float")
rex.bind("alt+cmd+m", "pane.minimize_floating")
rex.bind("alt+shift+cmd+enter", "pane.minimize_all_floating")
rex.bind("shift+cmd+enter", "pane.zoom")
rex.bind("shift+cmd+g", "pane.go_to_directory")
rex.bind("cmd+p", "pane.go_to")
rex.bind("ctrl+cmd+i", "pane.insert_path")
rex.bind("cmd+up", "client.prompt.previous")
rex.bind("cmd+down", "client.prompt.next")
rex.bind("cmd+y", "client.quick_look")

-- Find
rex.bind("cmd+f", "client.find.open")
rex.bind("cmd+g", "client.find.next")
rex.bind("shift+cmd+g", "client.find.previous")

-- View
rex.bind("shift+cmd+=", "client.font.increase")
rex.bind("cmd+=", "client.font.increase")
rex.bind("cmd+-", "client.font.decrease")
rex.bind("cmd+[Digit0]", "client.font.reset")
rex.bind("cmd+/", "client.sidebar.toggle")
rex.bind("cmd+.", "client.sidebar.activity.toggle")
rex.bind("shift+cmd+s", "client.tab_style.toggle")
rex.bind("alt+cmd+t", "client.toolbar.reveal")
rex.bind("alt+shift+cmd+t", "client.pane_header.reveal")
