;;; spaceline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "spaceline-config" "spaceline-config.el" (23363
;;;;;;  41186 104499 696000))
;;; Generated autoloads from spaceline-config.el

(autoload 'spaceline-spacemacs-theme "spaceline-config" "\
Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'.

\(fn &rest ADDITIONAL-SEGMENTS)" nil nil)

(autoload 'spaceline-emacs-theme "spaceline-config" "\
Install a modeline close to the one used by Spacemacs, but which
looks better without third-party dependencies.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'.

\(fn &rest ADDITIONAL-SEGMENTS)" nil nil)

(defvar spaceline-helm-mode nil "\
Non-nil if Spaceline-Helm mode is enabled.
See the command `spaceline-helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `spaceline-helm-mode'.")

(custom-autoload 'spaceline-helm-mode "spaceline-config" nil)

(autoload 'spaceline-helm-mode "spaceline-config" "\
Customize the mode-line in helm.

\(fn &optional ARG)" t nil)

(defvar spaceline-info-mode nil "\
Non-nil if Spaceline-Info mode is enabled.
See the command `spaceline-info-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `spaceline-info-mode'.")

(custom-autoload 'spaceline-info-mode "spaceline-config" nil)

(autoload 'spaceline-info-mode "spaceline-config" "\
Customize the mode-line in info.
This minor mode requires info+.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("spaceline-pkg.el" "spaceline-segments.el"
;;;;;;  "spaceline.el") (23363 41186 749965 479000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; spaceline-autoloads.el ends here
