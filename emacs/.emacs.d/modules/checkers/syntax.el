(require 'core-straight)

(package! flycheck
  :if (feature-p! +flycheck)
  :init
  (global-flycheck-mode 1)
  :custom-face
  (flycheck-info ((t (:underline (:style line :color "#80FF80")))))
  (flycheck-warning ((t (:underline (:style line :color "#FF9933")))))
  (flycheck-error ((t (:underline (:style line :color "#FF5C33")))))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        ;; flycheck is expensive to run all the time... annoying too
        flycheck-check-syntax-automatically '(mode-enabled save)
        ;; default numbers never make sense
        flycheck-display-errors-delay 0.25
        flycheck-checker-error-threshold 1500

        ;; For the above functionality, check syntax in a buffer that you switched to
        ;; only briefly. This allows "refreshing" the syntax check state for several
        ;; buffers quickly after e.g. changing a config file.
        flycheck-buffer-switch-check-intermediate-buffers t
        )
  ;; disable elisp checkdoc, it's way too annoying and needless for module files
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
