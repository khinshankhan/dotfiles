(require 'core-straight)
(require 'core-paths)
(require 'url-parse)

(package! vc
  :straight (:type built-in)
  :config
  ;; Remove RCS, CVS, SCCS, SRC, and Bzr, because it's a lot less work for vc to
  ;; check them all (especially in TRAMP buffers), and who uses any of these in
  ;; 2021, amirite?
  (setq-default vc-handled-backends '(Git SVN Hg)))

(package! vc-annotate
  :straight (:type built-in))

;; Permalinks to the forge, for the current line or region.
(defun shan--git-output (&rest args)
  "Run git with ARGS, returning trimmed stdout, or nil if it failed."
  (with-temp-buffer
    (when (zerop (apply #'process-file "git" nil t nil args))
      (let ((out (string-trim (buffer-string))))
        (unless (string-empty-p out) out)))))

(defun shan--git-remote-url-to-https (url)
  "Normalize a git remote URL into a browsable https URL.
Handles scp-style (git@host:org/repo.git), ssh:// and git:// remotes."
  (let ((url (replace-regexp-in-string "\\.git\\'" "" (string-trim url))))
    (cond
     ;; git@github.com:org/repo -> https://github.com/org/repo
     ((string-match "\\`[^/@]+@\\([^:]+\\):\\(.+\\)\\'" url)
      (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))
     ;; ssh://git@github.com/org/repo, git://github.com/org/repo
     ((string-match "\\`\\(?:ssh\\|git\\)://\\(?:[^/@]+@\\)?\\([^/]+\\)/\\(.+\\)\\'" url)
      (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))
     (t url))))

(defun shan--git-permalink-fragment (url start end)
  "Build the line-anchor fragment for URL, spanning START to END.
Only the host is inspected: an org or repo named e.g. \"bitbucket-migration\"
must not be mistaken for a Bitbucket forge."
  (let ((multi (and end (/= start end)))
        (host (or (url-host (url-generic-parse-url url)) "")))
    (cond
     ((string-match-p "bitbucket" host)
      (if multi (format "#lines-%d:%d" start end) (format "#lines-%d" start)))
     ;; github, gitlab, gitea, sourcehut and friends all agree on #Lx-Ly.
     (multi (format "#L%d-L%d" start end))
     (t (format "#L%d" start)))))

(defun shan/git-permalink (&optional beg end)
  "Copy a forge permalink to the current line to the kill ring.
When a region is active, link the whole span of lines instead.

The link pins the commit SHA rather than a branch so it does not rot,
and prefers the upstream remote of the current branch, falling back to
\\='origin\\='. With a prefix argument, the URL is also opened in a browser."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (when (file-remote-p file)
      (user-error "Cannot build a permalink for a remote file"))
    (let* ((default-directory (file-name-directory file))
           (root (or (shan--git-output "rev-parse" "--show-toplevel")
                     (user-error "Not inside a git repository")))
           (sha (or (shan--git-output "rev-parse" "HEAD")
                    (user-error "Repository has no commits to link to")))
           ;; Prefer wherever this branch actually tracks, else origin.
           (remote (or (car (split-string
                             (or (shan--git-output
                                  "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}")
                                 "")
                             "/" t))
                       "origin"))
           (url (shan--git-remote-url-to-https
                 (or (shan--git-output "remote" "get-url" remote)
                     (user-error "Remote %S has no URL" remote))))
           (relative (file-relative-name file (file-name-as-directory root)))
           (start (line-number-at-pos (or beg (point)) t))
           ;; A region ending at column 0 visually stops on the previous line.
           (finish (when end
                     (line-number-at-pos
                      (if (and (> end beg) (= (save-excursion (goto-char end) (current-column)) 0))
                          (1- end)
                        end)
                      t)))
           (link (concat url "/blob/" sha "/"
                         (mapconcat #'url-hexify-string
                                    (split-string relative "/")
                                    "/")
                         (shan--git-permalink-fragment url start finish))))
      (kill-new link)
      (when current-prefix-arg
        (browse-url link))
      (message "%s" link)
      link)))

(global-set-key (kbd "C-c g") #'shan/git-permalink)

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
  :init
  ;; Enforce git commit conventions.
  ;; See https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
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

(package! git-modes
  :if (feature-p! +git))

;; Gutters, mostly doom logic
(package! git-gutter
  :if (feature-p! +gutter)
  :commands git-gutter:revert-hunk git-gutter:stage-hunk git-gutter:previous-hunk git-gutter:next-hunk
  :init
  (defvar +vc-gutter-in-remote-files nil
    "If non-nil, enable git-gutter in remote/tramp buffers.")

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
