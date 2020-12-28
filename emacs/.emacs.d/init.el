;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      last-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(defun shan|revert-gc ()
  "Reset values and garbage collect."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist (append last-file-name-handler-alist
                                        file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal)
  (makunbound 'last-file-name-handler-alist)
  (garbage-collect))

(add-hook 'after-init-hook 'shan|revert-gc)

(defconst shan--config-dir (file-name-directory (file-chase-links load-file-name))
  "Directory where this file exists. Useful for generality in case of `load' or different paths.")

(setq package-enable-at-startup nil
      straight-use-package-by-default t
      straight-recipe-repositories nil
      straight-repository-branch "master"
      straight-fix-org nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; TODO: I still have some stray demands in my config, should figure that out sometime
(setq-default use-package-always-defer nil
	      use-package-always-demand t
	      byte-compile-warnings nil)
;; (setq use-package-verbose t)

(straight-use-package 'use-package)

(use-package no-littering
  :init
  (require 'no-littering))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package dash-functional
  :demand t)
(use-package f
  :demand t)
(use-package s
  :demand t)
(use-package string-inflection
  :demand t)

(require 'loadhist)
(require 'cl-seq)

(defconst custom-file (concat user-emacs-directory "custom.el"))
(defconst shan--settings-path (concat user-emacs-directory "personal/settings.el")
  "Path to personal settings meant not be public (api keys and stuff).")
(defconst shan--settings-exist? (file-exists-p shan--settings-path)
  "Checks if shan--settings-path exists.")

(if shan--settings-exist?
    (load-file shan--settings-path)
  (message "Settings file not found!"))

(defmacro shan!k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defmacro shan--no-hook (f hooks)
  "Call function F while temporarily removing HOOKS."
  `(lambda (&rest args)
     (let ((tbl (cl-loop for hook in ,hooks collect `(,(gensym) . ,hook))))
       (prog2
           (dolist (pair tbl)
             (eval `(setq ,(car pair) ,(cdr pair)))
             (eval `(setq ,(cdr pair) nil)))
           (apply ,f args)
         (dolist (pair tbl)
           (eval `(setq ,(cdr pair) ,(car pair))))))))

(defun shan/do-nothing ()
  "Do nothing."
  (interactive)
  nil)

(defun shan/before (to-call-before f)
  "Run TO-CALL-BEFORE then run F."
  (funcall to-call-before)
  (funcall f))

(defun shan/after (to-call-after f)
  "Run F then run TO-CALL-AFTER."
  (funcall f)
  (funcall to-call-after))

(defun shan/refresh-buffer ()
  "Refresh the current buffer."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun shan/scratch ()
  "Create a new scratch buffer to work in.  (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0) bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

(defun shan/sudo-edit (file-name)
  "Like find file, but opens FILE-NAME as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun shan/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun shan/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun shan/browser-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun shan/path-copy ()
  "Copy the current file path to kill ring."
  (interactive)
  (kill-new buffer-file-name))

(defun shan/add-list-to-list (to-list from-list &optional append compare-fn)
  "Add all elements from FROM-LIST to TO-LIST.  APPEND and COMPARE-FN work as they in `add-to-list'."
  (dolist (elem from-list)
    (add-to-list to-list elem append compare-fn))
  to-list)

(defun shan/copy-hooks-to (from-hook to-hook)
  "Copies one list of hooks to another, without the weird nonc circular list problem"
  (dolist (hook from-hook)
    (add-hook to-hook hook)))

(defun shan/vanilla-save ()
  "Save file without any hooks applied."
  (interactive)
  (funcall (shan--no-hook 'save-buffer '(before-save-hook after-save-hook))))

(defun shan/edit-config ()
  "Edit the configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "config.org")))

(defun shan/org-toc (&optional shan/file-name)
  "A nice search utility for org headers in a direcory."
  (interactive)
  (unless shan/file-name
    (setq shan/file-name (read-directory-name "Directory name: ")))
  (let ((files (f-entries shan/file-name (lambda (f) (f-ext? f "org")) t))
        (headlines '())
        choice)
    (loop for file in files do
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (cl-pushnew (list
                           (format "%-80s (%s)"
                                   (match-string 0)
                                   (file-name-nondirectory file))
                           :file file
                           :position (match-beginning 0))
                          headlines))))
    (setq choice
          (completing-read "Headline: " (reverse headlines)))
    (find-file (plist-get (cdr (assoc choice headlines)) :file))
    (goto-char (plist-get (cdr (assoc choice headlines)) :position))))

(defun shan/git-url-handler (url)
  "Hacky fix, if URL is ssh url, it will make it into https url or else return as is."
  (if (string-prefix-p "git" url)
      (concat "https://github.com/" (substring url 15))
    url))

(defun shan/browse-git-repo ()
  "Open repository with `browse-url' if applicable"
  (interactive)
  (let ((url (shan/git-url-handler (magit-get "remote.origin.url"))))
    (if (string-prefix-p "http" url)
        (browse-url url)
      (message "No remote repository at point!"))))

(defun shan/call-keymap (map &optional prompt)
  "Read a key sequence and call the command it's bound to in MAP."
  (let* ((help-form `(describe-bindings ,(vector map)))
         (key (read-key-sequence prompt))
         (cmd (lookup-key map key t)))
    (if (functionp cmd) (call-interactively cmd)
      (user-error "%s is undefined" key))))

(defun shan/exec-call-keymap (keymap prompt)
  "Executes `shan/call-keymap'"
  (interactive)
  (shan/call-keymap keymap prompt))

(defvar shan--k-gc-debug-p t
  "Boolean to determine whether to echo message for gc or not.")

(defvar shan--gc-timeout 45
  "Time limit for idleness until gc starts.")
(defvar shan--gc-timer nil
  "Timer which periodically runs gc logic. nil if not active.")

(defun shan|gc-collect()
  "Runs gc and outputs messages if debugging."
  (if shan--k-gc-debug-p
      (message "Garbage Collector has run for %.06fsec"
               (shan!k-time (garbage-collect)))
    (garbage-collect)))

(defun shan--gc-start ()
  "Start watching for when idle for shan--gc-timeout seconds to run the GC."
  (interactive)
  (unless shan--gc-timer
    (setq shan--gc-timer (run-with-idle-timer shan--gc-timeout t 'shan|gc-collect))))

(defun shan--gc-cancel ()
  "Stop idle gc."
  (interactive)
  (when shan--gc-timer
    (cancel-timer shan--gc-timer)
    (setq shan--gc-timer nil)))

(add-hook 'after-init-hook 'shan--gc-start)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(setq-default locale-coding-system 'utf-8)
(dolist (fn '(set-terminal-coding-system set-keyboard-coding-system set-selection-coding-system prefer-coding-system))
  (if (fboundp fn)
      (funcall fn 'utf-8)))

(use-package unidecode)

(setq-default backup-inhibited t
              auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

(setq inhibit-startup-message t)
(dolist (fn '(tool-bar-mode scroll-bar-mode menu-bar-mode))
  (if (fboundp fn)
      (funcall fn -1)))

(when (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :weight 'normal
                      :width 'normal))

(add-to-list 'face-ignored-fonts "Noto Color Emoji")

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(use-package doom-themes
  :demand t
  :config
  (setq doom-vibrant-brighter-comments t
        doom-vibrant-brighter-modeline t)
  (doom-themes-org-config)
  (load-theme 'doom-dracula t))

(use-package solaire-mode
  :demand t
  :functions persp-load-state-from-file
  :hook
  (prog-mode . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  (after-load-theme . solaire-mode-swap-bg)
  :config
  (setq solaire-mode-remap-modeline nil
        solaire-mode-remap-fringe nil)
  (solaire-global-mode 1)
  (solaire-mode-swap-bg)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))

(dolist (fn '(line-number-mode column-number-mode))
  (if (fboundp fn)
      (funcall fn t)))

(use-package doom-modeline
  :demand t
  :config
  (setq doom-modeline-python-executable "python3"
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-version t
        doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-mode))

(use-package default-text-scale
  :init
  (default-text-scale-mode))

(use-package zoom-window
  :bind
  ("C-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "#412170"))

(setq-default visible-bell nil
              audible-bell nil
              ring-bell-function 'ignore)

(defalias 'yes-or-no-p (lambda (&rest _) t))
(setq-default confirm-kill-emacs nil)
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(setq-default initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))

(setq-default blink-matching-paren nil
              visible-cursor nil
              x-stretch-cursor nil
              cursor-type 'box)

(use-package beacon
  :hook
  (focus-in . beacon-blink)
  :config
  (beacon-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config))

(use-package paren
  :demand t
  :config
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
  (show-paren-mode t))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package rainbow-mode
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))

  (define-globalized-minor-mode global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode 1)))
  (global-rainbow-mode 1))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(defun shan/fill-or-unfill ()
  "Fill or unfill based on the previous command."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(setq-default require-final-newline t
              vc-follow-symlinks t)

(global-subword-mode t)
(delete-selection-mode t)
(global-font-lock-mode t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(global-set-key [remap fill-paragraph]
                #'shan/fill-or-unfill)

(global-set-key (kbd "M-;")
                'comment-line)

(use-package which-key
  :init
  (which-key-mode 1))
;; :bind
;; ("C-h m" . which-key-show-major-mode)
;; ("C-h b" . which-key-show-top-level)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-x r t") 'mc/edit-lines)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key mc/keymap (kbd "<return>") nil))

(use-package key-chord
  :demand t
  :config
  (setq key-chord-two-keys-delay 0.05)
  (key-chord-mode t))

(use-package use-package-chords
  :demand t)

(use-package hydra
  :demand t
  :config
  (setq hydra--work-around-dedicated nil
        hydra-is-helpful t
        hydra-hint-display-type 'lv
        lv-use-separator nil)
  :chords
  ("ao" . hydra-leader/body))

(use-package pretty-hydra
  :demand t)

(pretty-hydra-define hydra-lsp (:exit t :color pink :title " LSP" :quit-key "q")
  ("Find"
   (("." lsp-ui-peek-find-references "find references")
    ("d" lsp-find-definition "find definition")
    ("t" lsp-find-type-definition "find type definition"))
   "Refactor"
   (("e" lsp-rename "rename symbol at point")
    ("f" lsp-format-buffer "format buffer"))
   "Show"
   (("j" lsp-ui-imenu "symbol table")
    ("l" lsp-ui-flycheck-list "error list"))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))

(use-package gitattributes-mode)
(use-package gitignore-mode)
(use-package gitconfig-mode)

(use-package magit
  :defer t
  :bind
  (:map magit-status-mode-map
        ("q" . (lambda () (interactive) (magit-mode-bury-buffer 16))))
  :config
  ;; allow window to be split vertically rather than horizontally
  (setq split-width-threshold 0)
  (setq split-height-threshold nil)
  ;; full window magit
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package transient
  :defer t
  :config
  (transient-bind-q-to-quit))

(defvar shan--ide-alist '()
  "List containing relationships of (mode . hydra).")

(defun shan--ide-add (mode hydra)
  "Add MODE and HYDRA as (mode . hydra) to `shan--ide-alist'."
  (push `(,mode . ,hydra) shan--ide-alist))

(defun shan--ide-resolve ()
  "Call a hydra related to active mode if found in `shan--ide-alist'."
  (interactive)
  (let ((hydra (alist-get major-mode shan--ide-alist)))
    (if hydra
        (funcall hydra)
      (message "IDE not found for %s" major-mode))))

(defconst shan--org-features '(org-macs org-compat ol ob-exp)
  "Features that may have been loaded by builtin Org but we want to use new Org's version.")
(defconst shan--reload-org-features-p (and (featurep 'org-macs) (s-contains? "usr" (feature-file 'org-macs)))
  "A bit hard-coded, but determines if we have to reload features due to builtin Org features being loaded.")

;; yeet built in Org path from load-path, so that a new Org path will definitely take precedence
(when-let (orglib (locate-library "org" nil load-path))
  (setq load-path (delete (substring (file-name-directory orglib) 0 -1)
                          load-path)))
(when shan--reload-org-features-p
  (dolist (org-feature shan--org-features)
    (and (featurep org-feature) (unload-feature org-feature t))))

(defun +org-fix-package-h (package &rest _)
  (when (equal package "org-mode")
    (let ((org-mode-dir (straight--repos-dir package)))
      (progn
        (message org-mode-dir)
        (apply 'f-mkdir (f-split org-mode-dir))
        (with-temp-file (expand-file-name "org-version.el" org-mode-dir)
          (insert "(fset 'org-release (lambda () \"9.4\"))\n"
                  "(fset 'org-git-version #'ignore)\n"
                  "(provide 'org-version)\n"))))))

(add-hook 'straight-use-package-pre-build-functions '+org-fix-package-h)

(use-package org
  :straight (org-mode
             :host github
             :repo "emacs-straight/org-mode"
             :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")))

(straight-use-package '(org :local-repo nil))

(when shan--reload-org-features-p
  (dolist (org-feature shan--org-features)
    (require org-feature)))

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-support-shift-select t)

(use-package toc-org
  :hook
  (org-mode . toc-org-enable))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("⁖"))
  (set-face-attribute 'org-level-1 nil
                      :height 1.25
                      :weight 'bold)
  (set-face-attribute 'org-level-2 nil
                      :height 1.1
                      :weight 'bold)
  (set-face-attribute 'org-level-3 nil
                      :height 1.0
                      :weight 'bold)
  (set-face-attribute 'org-level-4 nil
                      :height 1.0
                      :weight 'bold)

  (set-face-attribute 'org-ellipsis nil
                      :underline nil
                      :background "#fafafa"
                      :foreground "#a0a1a7"))

(use-package pubmed
  :commands (pubmed-search pubmed-advanced-search))

(setq browse-url-browser-function 'browse-url-generic)

(cond((executable-find "firefox") (setq browse-url-generic-args '("-private")
                                        browse-url-firefox-program "firefox"
                                        browse-url-generic-program "firefox"))
     ((executable-find "chromium") (setq browse-url-generic-args '("-incognito")
                                         browse-url-chromium-program "chromium"
                                         browse-url-generic-program "chromium"))
     ((executable-find "google-chrome") (setq browse-url-generic-args '("-incognito")
                                              browse-url-chrome-program "google-chrome"
                                              browse-url-generic-program "chrome")))

(use-package sicp)

(use-package wakatime-mode
  :if (and (executable-find "wakatime") (boundp 'wakatime-api-key))
  :config
  (setq wakatime-cli-path (executable-find "wakatime"))
  (global-wakatime-mode))

;; (use-package speed-type)
;; (use-package origami)
;; (use-package demangle-mode)
;; (use-package academic-phrases)
;; (use-package powerthesaurus)
;; (use-package crontab-mode)
;; (use-package salt-mode)
;; (use-package rmsbolt)                   ; A compiler output viewer
