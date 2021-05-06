(package! flycheck
  :init
  (global-flycheck-mode 1)
  :custom-face
  (flycheck-info ((t (:underline (:style line :color "#80FF80")))))
  (flycheck-warning ((t (:underline (:style line :color "#FF9933")))))
  (flycheck-error ((t (:underline (:style line :color "#FF5C33")))))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))
