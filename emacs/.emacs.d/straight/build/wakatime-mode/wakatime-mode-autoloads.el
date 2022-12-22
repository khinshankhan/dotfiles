;;; wakatime-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "wakatime-mode" "wakatime-mode.el" (0 0 0 0))
;;; Generated autoloads from wakatime-mode.el

(autoload 'wakatime-mode "wakatime-mode" "\
Toggle WakaTime (WakaTime mode).

This is a minor mode.  If called interactively, toggle the
`wakatime mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `wakatime-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-wakatime-mode 'globalized-minor-mode t)

(defvar global-wakatime-mode nil "\
Non-nil if Global Wakatime mode is enabled.
See the `global-wakatime-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-wakatime-mode'.")

(custom-autoload 'global-wakatime-mode "wakatime-mode" nil)

(autoload 'global-wakatime-mode "wakatime-mode" "\
Toggle Wakatime mode in all buffers.
With prefix ARG, enable Global Wakatime mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Wakatime mode is enabled in all buffers where `(lambda nil (wakatime-mode 1))' would do it.

See `wakatime-mode' for more information on Wakatime mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "wakatime-mode" '("s-blank" "wakatime-"))

;;;***

(provide 'wakatime-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wakatime-mode-autoloads.el ends here
