(require 'core-straight)
(require 'core-fboundp)

;; Git
(package! git-modes
  :if (feature-p! +git))

;;; Some people download emacs just for magit, it’d be crazy to not use it for vc.
(package! magit
  :if (feature-p! +git)
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
  :if (feature-p! +git)
  :defer t
  :config
  (transient-bind-q-to-quit))

;;; Some optimization for git projects.
(package! vc-hooks
  :straight (:type built-in)
  :config
  ;; I mostly use Git, makes sense to have it at the beginning of the list
  (setq vc-handled-backends
        (cons 'Git (remove 'Git vc-handled-backends))))

;; Forge
;;; TODO: figure out +forge https://github.com/magit/forge
;; Gutters
;;; TODO: figure out +vc-gutter https://github.com/emacsorphanage/git-gutter
