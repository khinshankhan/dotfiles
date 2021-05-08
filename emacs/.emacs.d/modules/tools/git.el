;;; git.el --- git
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! magit
  :defer t
  :bind
  ("C-c g" . magit)
  (:map magit-status-mode-map
        ("q" . (lambda () (interactive) (magit-mode-bury-buffer 16))))
  :config
  ;; allow window to be split vertically rather than horizontally
  (setq split-width-threshold 0)
  (setq split-height-threshold nil)
  ;; full window magit
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(package! transient
  :defer t
  :config
  (transient-bind-q-to-quit))

(package! vc-hooks
  :straight (:type built-in)
  :config
  ;; I mostly use Git, makes sense to have it at the beginning of the list
  (setq vc-handled-backends
        (cons 'Git (remove 'Git vc-handled-backends))))

(package! transient
  :config
  (transient-bind-q-to-quit))

(package! gitattributes-mode
  :mode "\\.gitattributes\\'")
(package! gitignore-mode
  :mode "\\.gitignore\\'")
(package! gitconfig-mode
  :mode "\\.gitconfig\\'")

(provide 'git)
;;; git.el ends here
