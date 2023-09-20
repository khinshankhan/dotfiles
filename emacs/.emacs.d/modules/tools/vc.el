(require 'core-straight)
(require 'core-paths)

(package! vc
  :straight (:type built-in)
  :config
  ;; Remove RCS, CVS, SCCS, SRC, and Bzr, because it's a lot less work for vc to
  ;; check them all (especially in TRAMP buffers), and who uses any of these in
  ;; 2021, amirite?
  (setq-default vc-handled-backends '(Git SVN Hg)))

(package! vc-annotate
  :straight (:type built-in))

(package! smerge-mode
  :straight (:type built-in))

;; Git
;;; Some people download emacs just for magit, it’d be crazy to not use it for vc.
(package! magit
  :if (feature-p! +git)
  :defer t
  :init
  (defun magit-custom/true-buffer-bury()
    "Get rid of buffers for realsies."
    (interactive)
    (magit-mode-bury-buffer t))
  :bind
  ("C-c m" . magit)
  (:map magit-status-mode-map
        ("q" . magit-custom/true-buffer-bury))
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
    '("-a" "Autostash" "--autostash")))

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

(package! git-commit
  :if (feature-p! +git)
  :config
  ;; Enforce git commit conventions.
  ;; See https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))

  (global-git-commit-mode t))

(package! git-modes
  :if (feature-p! +git))

;; Gutters, mostly doom logic
(package! git-gutter
  :if (feature-p! +gutter)
  :commands git-gutter:revert-hunk git-gutter:stage-hunk git-gutter:previous-hunk git-gutter:next-hunk
  :init
  (defun +vc-gutter-init-maybe-h ()
    "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
    (let ((file-name (buffer-file-name (buffer-base-buffer))))
      (cond
       ((and (file-remote-p (or file-name default-directory))
             (not +vc-gutter-in-remote-files)))
       ;; UX: If not a valid file, wait until it is written/saved to activate
       ;;   git-gutter.
       ((not (and file-name (vc-backend file-name)))
        (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
       ;; UX: Allow git-gutter or git-gutter-fringe to activate based on the
       ;;   type of frame we're in. This allows git-gutter to work for silly
       ;;   geese who open both tty and gui frames from the daemon.
       ((if (and (display-graphic-p)
                 (require 'git-gutter-fringe nil t))
            (setq-local git-gutter:init-function      #'git-gutter-fr:init
                        git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                        git-gutter:clear-function     #'git-gutter-fr:clear
                        git-gutter:window-width -1)
          (setq-local git-gutter:init-function      'nil
                      git-gutter:view-diff-function #'git-gutter:view-diff-infos
                      git-gutter:clear-function     #'git-gutter:clear-diff-infos
                      git-gutter:window-width 1))
        (unless (memq major-mode git-gutter:disabled-modes)
          (git-gutter-mode +1)
          (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local))))))
  (add-hook 'find-file-hook #'+vc-gutter-init-maybe-h)

  ;; UX: Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;;   syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;;   global minor modes gets called for new buffers while they are still in
  ;;   `fundamental-mode', before a major mode has been assigned. I don't know
  ;;   why this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  ;; PERF: Only enable the backends that are available, so it doesn't have to
  ;;   check when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; UX: update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (setq git-gutter:update-interval 0.05))

;; NOTE: flycheck errors take priority in fringe
(package! git-gutter-fringe
  :if (feature-p! +gutter)
  :config
  ;; appearance of gutters (prefer solid lines over symbols)
  (define-fringe-bitmap 'git-gutter-fr:added [#b11100000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11100000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b11100000] nil nil '(center repeated)))

(package! diff-hl
  :if (feature-p! +gutter))
