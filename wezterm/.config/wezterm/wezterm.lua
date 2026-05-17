local wezterm = require("wezterm")
local config = wezterm.config_builder()
local is_macos = wezterm.target_triple:find("darwin") ~= nil

config.font = wezterm.font_with_fallback({
	"Hack Nerd Font Mono",
	"JetBrainsMono Nerd Font Mono",
	"Fira Code",
	"Source Code Pro",
	"monospace",
})
config.font_size = 16
config.line_height = 1.0
config.cell_width = 1.0

config.color_scheme = "Builtin Dark"
config.colors = {
	background = "#272a30",
	foreground = "#d4d4d4",
	cursor_bg = "#d4d4d4",
	cursor_fg = "#272a30",
	selection_bg = "#3d4148",
	selection_fg = "none",
	ansi = { "#1e2127", "#e06c75", "#98c379", "#e5c07b", "#61afef", "#c678dd", "#56b6c2", "#abb2bf" },
	brights = { "#5c6370", "#e06c75", "#98c379", "#e5c07b", "#61afef", "#c678dd", "#56b6c2", "#ffffff" },
	tab_bar = {
		background = "#21242a",
		active_tab = { bg_color = "#272a30", fg_color = "#d4d4d4" },
		inactive_tab = { bg_color = "#21242a", fg_color = "#5c6370" },
		inactive_tab_hover = { bg_color = "#2c3038", fg_color = "#d4d4d4" },
		new_tab = { bg_color = "#21242a", fg_color = "#5c6370" },
		new_tab_hover = { bg_color = "#2c3038", fg_color = "#d4d4d4" },
	},
}

config.window_decorations = "RESIZE"
config.window_padding = { left = 8, right = 8, top = 8, bottom = 4 }

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.hide_tab_bar_if_only_one_tab = false
config.tab_max_width = 32

config.default_cursor_style = "SteadyBar"
config.cursor_blink_rate = 0

config.scrollback_lines = 10000
config.enable_scroll_bar = false
config.adjust_window_size_when_changing_font_size = false
config.audible_bell = "Disabled"
config.max_fps = 120
config.animation_fps = 120
config.front_end = "WebGpu"
config.enable_kitty_keyboard = true
config.scroll_to_bottom_on_input = true

if is_macos then
	config.send_composed_key_when_left_alt_is_pressed = false
	config.send_composed_key_when_right_alt_is_pressed = false
end

local mod = is_macos and "SUPER" or "CTRL|SHIFT"
local mod_shift = is_macos and "SUPER|SHIFT" or "CTRL|SHIFT|ALT"

local act = wezterm.action
config.keys = {
	{ key = "d", mods = mod, action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "d", mods = mod_shift, action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "w", mods = mod, action = act.CloseCurrentPane({ confirm = true }) },
	{ key = "[", mods = mod, action = act.ActivatePaneDirection("Prev") },
	{ key = "]", mods = mod, action = act.ActivatePaneDirection("Next") },
	{ key = "z", mods = mod_shift, action = act.TogglePaneZoomState },
	{ key = "f", mods = is_macos and "CTRL|SUPER" or "CTRL|SHIFT", action = act.ToggleFullScreen },
	{ key = "k", mods = mod, action = act.ClearScrollback("ScrollbackAndViewport") },
}

for i = 1, 9 do
	table.insert(config.keys, { key = tostring(i), mods = mod, action = act.ActivateTab(i - 1) })
end

return config
