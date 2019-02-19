;;; winum-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "winum" "winum.el" (23363 41198 392754 902000))
;;; Generated autoloads from winum.el

(defvar winum-mode nil "\
Non-nil if Winum mode is enabled.
See the command `winum-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `winum-mode'.")

(custom-autoload 'winum-mode "winum" nil)

(autoload 'winum-mode "winum" "\
A minor mode that allows for managing windows based on window numbers.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-0-or-10 "winum" "\
Jump to window 0 if assigned or 10 if exists.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-0 "winum" "\
Jump to window 0.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-1 "winum" "\
Jump to window 1.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-2 "winum" "\
Jump to window 2.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-3 "winum" "\
Jump to window 3.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-4 "winum" "\
Jump to window 4.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-5 "winum" "\
Jump to window 5.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-6 "winum" "\
Jump to window 6.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-7 "winum" "\
Jump to window 7.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-8 "winum" "\
Jump to window 8.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-9 "winum" "\
Jump to window 9.
If prefix ARG is given, delete the window instead of selecting it.

\(fn &optional ARG)" t nil)

(autoload 'winum-select-window-by-number "winum" "\
Select or delete window which number is specified by ARG.
If the number is negative, delete the window instead of selecting it.
There are several ways to provide the number:
- if called from elisp with an argument, use it.
- if called interactively with a numeric prefix argument, use it.
- if prefix argument is the negative argument, delete window 0.
- if prefix argument is the default prefix argument, delete current window.
- if called interactively and no valid argument is provided, read from
  minibuffer.

\(fn &optional ARG)" t nil)

(autoload 'winum-set-keymap-prefix "winum" "\
Set key bindings prefix for `winum-keymap' based on `winum-base-map'.
This function overrides the value of `winum-keymap', so you
should call it before customization of `winum-keymap' and/or
after customization of `winum-base-map'.
PREFIX must be a key sequence, like the ones returned by `kbd'.

\(fn PREFIX)" nil nil)

(autoload 'winum-get-window-by-number "winum" "\
Return window numbered N if exists, nil otherwise.

\(fn N)" nil nil)

(autoload 'winum-get-number-string "winum" "\
Get the current or specified window's current number as a propertized string.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned.

\(fn &optional WINDOW)" nil nil)

(autoload 'winum-get-number "winum" "\
Get the current or specified window's current number.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned.

\(fn &optional WINDOW)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; winum-autoloads.el ends here
