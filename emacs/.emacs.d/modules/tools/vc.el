(require 'core-straight)
(require 'core-fboundp)

;; Git
(package! git-modes
  :if (feature-p! +git))

;;; Some people download emacs just for magit, it’d be crazy to not use it for vc.
(package! magit
  :if (feature-p! +git)
  :defer t
  :init
  (defun shan/magit-true-buffer-bury()
    "Get rid of buffers for realsies."
    (interactive)
    (magit-mode-bury-buffer t))
  :bind
  ("C-c g" . magit)
  (:map magit-status-mode-map
        ("q" . shan/magit-true-buffer-bury))
  :config
  (setq magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil
        ;; allow window to be split vertically rather than horizontally
        split-width-threshold 0
        split-height-threshold nil
        ;; full window magit
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  )

(package! transient
  :if (feature-p! +git)
  :defer t
  :init
  ;; Must be set early to prevent ~/.config/emacs/transient from being created
  (setq transient-levels-file  (concat shan-cache-dir "/transient/levels")
        transient-values-file  (concat shan-cache-dir "/transient/values")
        transient-history-file (concat shan-cache-dir "/transient/history"))
  :config
  (transient-bind-q-to-quit)
  (setq transient-default-level 5))

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
