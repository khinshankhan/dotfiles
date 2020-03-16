;; -*- lexical-binding: t -*-

(eval-and-compile
  (defun srs|revert-gc ()
    ;; reset values
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist (append last-file-name-handler-alist
		                                  file-name-handler-alist))
    ;; delete any duplicate values
    (cl-delete-duplicates file-name-handler-alist :test 'equal)
    ;; get rid of temporarily variables
    (makunbound 'default-file-name-handler-alist))

  ;; set everything to efficient limits and save values
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        last-file-name-handler-alist file-name-handler-alist
        file-name-handler-alist nil)

  (add-hook 'after-init-hook 'srs|revert-gc))

(require 'package)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :config
  (setq-default use-package-always-defer nil
                use-package-always-ensure t
                use-package-always-demand t
                byte-compile-warnings nil))

(use-package use-package-ensure-system-package)

(use-package try)

(use-package bug-hunter)

(use-package dash)
(use-package dash-functional)
(use-package f)
(use-package s)

(defconst custom-file (concat user-emacs-directory "custom.el"))
(defconst shan/settings-path (concat user-emacs-directory "personal/settings.el"))
(defconst shan/settings-exist? (file-exists-p shan/settings-path))
(when shan/settings-exist?
  (load-file shan/settings-path))

(defconst shan/personal? (-contains? '("shan" "faux-thunkpad") (system-name)))
(defconst shan/is-mac? (memq window-system '(mac ns)))

(defconst shan/preferred-logo "personal/nezuko-emacs.png")
(defconst shan/elfeed-file (concat user-emacs-directory "personal/elfeed.org"))
(defconst shan/elfeed-db (concat user-emacs-directory "personal/elfeeddb"))

(defconst shan/python-executable "python3")
(defconst shan/ipython-executable "ipython3")

(setq shan/home-row
      (if shan/personal?
          '(?a ?r ?s ?t ?n ?e ?i ?o)
        '(?a ?s ?d ?f ?j ?k ?l ?\;)))

(defconst shan/dart-path "/opt/flutter/bin/cache/dart-sdk/")
(defconst shan/flutter-path "/opt/flutter/")
(defconst shan/plantuml-path "/usr/share/java/plantuml/plantuml.jar")
(defconst shan/kotlin-path "/home/shan/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")

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

(defun shan/fill-or-unfill ()
  "Fill or unfill based on the previous command."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun shan/add-list-to-list (to-list from-list &optional append compare-fn)
  "Adds all elements from from-list to to-list"
  (dolist (elem from-list)
    (add-to-list to-list elem append compare-fn))
  to-list)

(defun shan/copy-hooks-to (from-hook to-hook)
  "Copies one list of hooks to another, without the weird nonc circular list problem"
  (dolist (hook from-hook)
    (add-hook to-hook hook)))

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

(defun shan/reload ()
  "Reload the configuration file."
  (interactive)
  (load-file shan/init-path))

(defun shan/edit-config ()
  "Edit the configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "config.org")))

(use-package vterm)
(use-package vterm-toggle
  :config
  ;; I like vterm to 'pop up' on the bottom
  ;; if anything, I can use zoom-window-zoom to focus
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(defun shan/vterm-helper ()
  (interactive)
  (if (string-equal (buffer-name) "vterm")
      (progn
        (kill-buffer "vterm")
        (delete-window))
    (vterm-toggle-cd)))

(setq-default locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package unidecode)

(setq-default backup-inhibited t
              auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

(when (>= emacs-major-version 26)
  (setq-default confirm-kill-processes nil))

(bind-key* "C-;" 'company-yasnippet)
(windmove-default-keybindings 'meta)

(use-package which-key
  :config
  (which-key-mode)
  :bind
  ;; ("C-h m" . which-key-show-major-mode)
  ;; ("C-h b" . which-key-show-top-level)
  )

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-x r t") 'mc/edit-lines)
  (define-key mc/keymap (kbd "<return>") nil))

(use-package use-package-chords
  :after (key-chord))

(use-package key-chord
  :custom
  (key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode t))

(use-package hydra
  :custom
  (hydra--work-around-dedicated nil)
  (hydra-is-helpful t)
  (hydra-hint-display-type 'lv)
  (lv-use-separator nil)
  :chords
  ("ao" . hydra-leader/body))

(use-package pretty-hydra
  :after (hydra))

(pretty-hydra-define hydra-config (:exit t :color pink :title " Personal" :quit-key "q")
  (" Configuration"
   (("e" shan/edit-config "config file")
    ("r" shan/reload "reload")
    ("s" (shan/org-toc (concat user-emacs-directory "config/.")) "search config"))
   "Utility"
   (("g" shan/refresh-buffer "refresh buffer"))
   " Exit"
   (("<deletechar>" save-buffers-kill-terminal "quit emacs")
    ("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))

(pretty-hydra-define hydra-help (:exit t :color pink :title " Help" :quit-key "q")
  ("Bindings"
   (("b" counsel-descbinds "all")
    ("m" which-key-show-major-mode "major mode"))
   "Describes"
   (("f" counsel-describe-function "function")
    ("k" describe-key "key")
    ("v" counsel-describe-variable "variable"))
   "Others"
   (("F" counsel-describe-face "face")
    ("l" view-lossage "command history"))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))

(pretty-hydra-define hydra-projectile (:exit t :color pink :title " Projectile" :quit-key "q")
  (""
   (("a" projectile-find-other-file "find other file")
    ("b" projectile-switch-to-buffer "switch buffer")
    ("c" projectile-compile-project "compile")
    ("d" projectile-find-dir "find directory"))
   ""
   (("e" projectile-recentf "recent files")
    ("f" projectile-find-file "find file")
    ("g" projectile-grep "grep")
    ("k" projectile-kill-buffers "kill project buffers"))
   ""
   (("p" projectile-switch-project "switch project")
    ("t" projectile-toggle-between-implementation-and-test "impl ↔ test")
    ("v" projectile-vc "version control"))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))

(pretty-hydra-define hydra-avy (:exit t :color pink :title " Avy" :quit-key "q")
  ("Goto"
   (("c" avy-goto-char-timer "timed char")
    ("C" avy-goto-char "char")
    ("w" avy-goto-word-1 "word")
    ("W" avy-goto-word-0 "word*")
    ("l" avy-goto-line "bol")
    ("L" avy-goto-end-of-line "eol"))
   "Line"
   (("m" avy-move-line "move")
    ("k" avy-kill-whole-line "kill")
    ("y" avy-copy-line "yank"))
   "Region"
   (("M" avy-move-region "move")
    ("K" avy-kill-region "kill")
    ("Y" avy-copy-region "yank"))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))

(pretty-hydra-define hydra-window (:exit nil :color pink :title " Screen" :quit-key "q")
  ("Window Split"
   (("2" split-window-below "below")
    ("3" split-window-right "right"))
   "Window Movement"
   (("c" ace-window "choose" :exit t)
    ("b" balance-windows "balance")
    ("l" delete-window "kill" :exit t)
    ("w" other-window "move"))
   "Buffer Movement"
   (("k" kill-buffer "kill" :exit t))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold) :exit t))))

(pretty-hydra-define hydra-file (:exit t :color pink :title " Files" :quit-key "q")
  ("Private"
   ()
   "Find"
   (("f" counsel-find-file "find")
    ("s" shan/sudo-edit "sudo")
    ("d" dired "dired"))
   "Operations"
   (("r" shan/rename-this-file-and-buffer "rename")
    ("y" shan/path-copy "yank path")
    ("k" shan/delete-this-file "delete file")
    ("b" shan/browser-current-file "browser"))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold) :exit t))))

(pretty-hydra-define hydra-git (:exit nil :color pink :title " Git" :quit-key "q")
  ("Commands"
   (("g" magit "magit" :exit t)
    ("t" git-timemachine "timemachine" :exit t))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold) :exit t))))

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

(pretty-hydra-define hydra-leader (:exit t :color pink :title " Leader" :quit-key "q")
  ("General"
   (("RET" hydra-config/body (propertize "+config" 'face 'bold))
    ("SPC" shan/ide-resolve (propertize "+ide" 'face 'bold))
    ("h" hydra-help/body (propertize "+help" 'face 'bold))
    ("t" shan/vterm-helper "terminal"))
   "Short Hands"
   (("f" hydra-file/body (propertize "+file" 'face 'bold))
    ("g" hydra-git/body (propertize "+git" 'face 'bold))
    ("i" ibuffer "ibuffer")
    ("r" shan/toggle-mark-rectangle "rectangle"))
   "Shortcuts"
   (("p" hydra-projectile/body (propertize "+project" 'face 'bold))
    ("j" hydra-avy/body (propertize "+jump" 'face 'bold))
    ("w" hydra-window/body (propertize "+screen" 'face 'bold)))
   "RSI Binds"
   (("u" undo "undo" :exit nil)
    ("a" (shan/exec-call-keymap 'Control-X-prefix "C-x") "C-x")
    (";" counsel-M-x "M-x")
    ("s" save-buffer "save"))))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(cond ((member "Source Code Pro" (font-family-list))
       (set-face-attribute 'default nil
                           :family "Source Code Pro"
                           :weight 'normal
                           :width 'normal)))

(add-to-list 'face-ignored-fonts "Noto Color Emoji")

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

(use-package zerodark-theme
  :defer t)

(use-package poet-theme
  :defer t)

(use-package doom-themes
  :custom
  (doom-vibrant-brighter-comments t)
  (doom-vibrant-brighter-modeline t)
  :config
  (doom-themes-org-config)
  (load-theme 'doom-dracula t))

(use-package solaire-mode
  :functions persp-load-state-from-file
  :hook
  (prog-mode . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  (after-load-theme . solaire-mode-swap-bg)
  :custom
  (solaire-mode-remap-modeline nil)
  (solaire-mode-remap-fringe nil)
  :config
  (solaire-global-mode 1)
  (solaire-mode-swap-bg)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))

(line-number-mode t)
(column-number-mode t)

(use-package doom-modeline
  :custom
  (doom-modeline-python-executable shan/python-executable)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-version t)
  (doom-modeline-buffer-file-name-style 'file-name)
  :config
  (doom-modeline-mode))

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode ibuffer-mode help-mode deft-text-mode) . hide-mode-line-mode))

(use-package default-text-scale
  :init
  (default-text-scale-mode))

(use-package zoom-window
  :config
  (global-set-key (kbd "C-z") 'zoom-window-zoom))

(setq-default visible-bell nil
              audible-bell nil
              ring-bell-function 'ignore)

(defalias 'yes-or-no-p (lambda (&rest _) t))
(setq-default confirm-kill-emacs nil)
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(setq-default transient-mark-mode t
              visual-line-mode t
              indent-tabs-mode nil
              tab-width 4)

(global-hl-line-mode t)

(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-character ?\┊);;?\間)
  )

;;(setq-default initial-major-mode 'python-mode)
(setq-default initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)

(setq-default scroll-margin 0
              scroll-conservatively 10000
              scroll-preserve-screen-position t
              mouse-wheel-progressive-speed nil)

(use-package page-break-lines)

(use-package dashboard
  :bind
  (:map dashboard-mode-map
        ("n" . widget-forward)
        ("p" . widget-backward)
        ("f" . shan/elfeed-update-database))
  :custom
  (dashboard-banner-logo-title
   (format ""
           (float-time (time-subtract after-init-time before-init-time))
           (length package-activated-list) gcs-done))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-center-content t)
  (dashboard-set-footer nil)

  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `((

      (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       ""
       "GH Repos"
       (lambda (&rest _) (browse-url-generic "https://github.com/kkhan01?tab=repositories")))

      (,(all-the-icons-material "update" :height 1.2 :v-adjust -0.24)
       ""
       "Update emacs"
       (lambda (&rest _) (shan/elfeed-update-database)))

      (,(all-the-icons-material "autorenew" :height 1.2 :v-adjust -0.15)
       ""
       "Restart emacs"
       (lambda (&rest _) (shan/reload)))

      )))

  :config
  (setq dashboard-items '((recents  . 5)
                          ;; (bookmarks . 5)
                          ;; (projects . 5)
                          (agenda . 5)
                          ;; (registers . 5)
                          ))

  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (if shan/settings-exist?
                                     (concat user-emacs-directory shan/preferred-logo)
                                   'logo))

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package ivy
  :bind
  ([switch-to-buffer] . ivy-switch-buffer)
  (:map ivy-minibuffer-map
        ([remap xref-find-definitions] . shan/do-nothing)
        ([remap xref-find-definitions-other-frame] . shan/do-nothing)
        ([remap xref-find-definitions-other-window] . shan/do-nothing)
        ([remap xref-find-references] . shan/do-nothing)
        ([remap xref-find-apropos] . shan/do-nothing)
        ("<return>" . ivy-alt-done)
        ("<S-return>" . ivy-immediate-done))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-height 20)
  (ivy-display-style 'fancy)
  (ivy-format-function 'ivy-format-function-line)
  (ivy-re-builders-alist
   '((t . ivy--regex-plus)))
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  ("C-x b" . counsel-switch-buffer)
  :config
  ;; weird because of a top-level push in source cod
  (setq-default ivy-initial-inputs-alist nil))

(use-package swiper
  :bind
  ("C-s" . swiper-isearch)
  ("C-r" . swiper-isearch-backward))

(use-package neotree
  :after
  (projectile)
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :custom
  (neo-theme 'nerd2)
  (neo-window-position 'left)
  :bind
  ([f8] . neotree-current-dir-toggle)
  ([f9] . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))

  (defun neotree-current-dir-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
             (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

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
  :config
  (show-paren-mode t))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package company
  :bind
  ("C-/" . company-complete)
  (:map company-active-map
        ("M-/" . company-other-backend)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :custom-face
  (company-tooltip ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-tooltip-annotation ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-tooltip-selection ((t (:foreground "#abb2bf" :background "#393f49"))))
  (company-tooltip-mouse ((t (:background "#30343c"))))
  (company-tooltip-common ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-tooltip-common-selection ((t (:foreground "#abb2bf" :background "#393f49"))))
  (company-preview ((t (:background "#30343c"))))
  (company-preview-common ((t (:foreground "#abb2bf" :background "#30343c"))))
  (company-scrollbar-fg ((t (:background "#30343c"))))
  (company-scrollbar-bg ((t (:background "#30343c"))))
  (company-template-field ((t (:foreground "#282c34" :background "#c678dd"))))
  :custom
  (company-require-match 'never)
  (company-dabbrev-downcase nil)
  (company-tooltip-align-annotations t)
  (company-idle-delay 3) ;; 128)
  (company-minimum-prefix-length 3) ;; 128)
  :config
  (global-company-mode t))

(use-package company-quickhelp
  :after (company)
  :config
  (company-quickhelp-mode))

(use-package company-box
  :after (company)
  :hook
  (company-mode . company-box-mode))

(use-package flycheck
  :custom-face
  (flycheck-info ((t (:underline (:style line :color "#80FF80")))))
  (flycheck-warning ((t (:underline (:style line :color "#FF9933")))))
  (flycheck-error ((t (:underline (:style line :color "#FF5C33")))))
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b11111110
            #b11111110
            #b11111110
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'info
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info
    :info-list-face 'flycheck-error-list-info)
  (flycheck-define-error-level 'warning
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning
    :warning-list-face 'flycheck-error-list-warning)
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)
  (global-flycheck-mode t))

(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))

(setq js2-missing-semi-one-line-override t)
(setq js2-strict-missing-semi-warning nil)

(use-package gitattributes-mode
  :defer t)
(use-package gitignore-mode
  :defer t)
(use-package gitconfig-mode
  :defer t)

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
  :after magit
  :config
  (transient-bind-q-to-quit))

(use-package forge)

(use-package git-timemachine)

(use-package projectile
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '("~/Projects/"))
  ;; ignore set up: https://www.youtube.com/watch?v=qpv9i_I4jYU
  (projectile-indexing-method 'hybrid)
  (projectile-sort-order 'access-time)
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode t))

(use-package counsel-projectile
  :disabled
  :after
  (counsel projectile)
  :config
  (counsel-projectile-mode t)
  (defalias 'projectile-switch-to-buffer 'counsel-projectile-switch-to-buffer)
  (defalias 'projectile-find-dir 'counsel-projectile-find-dir)
  (defalias 'projectile-find-file 'counsel-projectile-find-file)
  (defalias 'projectile-grep 'counsel-projectile-grep)
  (defalias 'projectile-switch-project 'counsel-projectile-switch-project))

(use-package tramp
  :ensure nil
  :config
  ;; faster than scp
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-default-user-alist
               '("ssh" "eniac.*.edu\\'" "Khinshan.Khan44") ;; current eniac logins
               '(nil nil "shan")) ;; fallback login

  (setq password-cache-expiry nil))

;; this hook makes remote projectile a little lighter
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile"))))

(use-package tramp-term)

(defvar shan/ide--alist '())

(defun shan/ide-add (mode hydra)
  (push `(,mode . ,hydra) shan/ide--alist))

(defun shan/ide-resolve ()
  (interactive)
  (let ((hydra (alist-get major-mode shan/ide--alist)))
    (if hydra
        (funcall hydra)
      (message "IDE not found for %s" major-mode))))

(use-package lsp-mode
  :custom
  (lsp-auto-guess-root t)
  (lsp-before-save-edits t)
  (lsp-enable-indentation t)
  (lsp-auto-configure t)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  :config
  (require 'lsp-clients) ;; due to lsp-auto-configure being nil
  (setq lsp-print-io t))

(use-package lsp-ui
  :after (lsp-mode)
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-flycheck-enable t))

(use-package company-lsp
  :after (company lsp-mode)
  :bind
  (:map lsp-mode-map
        ("C-/" . company-lsp))
  :custom
  (company-lsp-async t)
  (company-lsp-cache-candidates t)
  (company-lsp-enable-snippets nil)
  (company-lsp-enable-recompletion t)
  :config
  (add-to-list 'company-backends #'company-lsp))

(use-package dap-mode
  :after (hydra)
  :hook
  (lsp-mode . (lambda () (dap-mode t) (dap-ui-mode t)))
  :config
  (use-package dap-hydra
    :ensure nil
    :config
    (defhydra+ dap-hydra (:exit nil :foreign-keys run)
      ("d" dap-debug "Start debug session"))
    (pretty-hydra-define+ hydra-lsp ()
      (;; these heads are added to the existing " Exit" column
       " Exit"
       (("SPC" dap-hydra "dap"))))))

(use-package treemacs
  :bind (:map global-map
              ("C-x t t" . treemacs)
              ("C-x t 1" . treemacs-select-window))
  :config
  (setq treemacs-resize-icons 4))

(use-package lsp-treemacs
  :init (lsp-treemacs-sync-mode 1)
  :bind (:map java-mode-map
              ("C-x e l" . lsp-treemacs-errors-list)
              ("C-x s l" . lsp-treemacs-symbols)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package asm-mode
  :mode "\\.as\\'"
  :bind (:map asm-mode-map
		      ("<f5>" . #'compile)))

(use-package mips-mode
  :mode "\\.mips$")

(use-package company-c-headers
  :after (company)
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package cc-mode
  :ensure nil
  :hook
  ((c-mode c++-mode) . lsp)
  :custom
  (c-basic-offset 4)
  :config
  (setq c-default-style '((c++-mode  . "stroustrup")
                          (awk-mode  . "awk")
                          (java-mode . "java")
                          (other     . "k&r")))
  (shan/ide-add 'c-mode #'hydra-lsp/body)
  (shan/ide-add 'c++-mode #'hydra-lsp/body))

(use-package dap-gdb-lldb
  :ensure nil
  :after (dap-mode))

(use-package modern-cpp-font-lock
  :hook
  (c++-mode . modern-c++-font-lock-mode))

(use-package google-c-style
  :hook
  ((c-mode c++-mode) . google-set-c-style)
  (c-mode-common . google-make-newline-indent))

(use-package clojure-mode)

(use-package cider
  :bind
  (:map cider-repl-mode-map
        ("C-l" . cider-repl-clear-buffer))
  :custom
  (cider-print-fn 'fipp)
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-in-current-window nil)
  (cider-font-lock-dynamically t))

(use-package elein)

(use-package dart-mode
  :hook
  (dart-mode . lsp)
  :custom
  (dart-format-on-save t)
  (dart-sdk-path shan/dart-path))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path shan/flutter-path))

(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))

(use-package elixir-mode
  :init
  (add-hook 'elixir-mode-hook #'company-mode))

(use-package alchemist)

(use-package go-mode
  :if (and (executable-find "go") (executable-find "bingo"))
  :hook
  (go-mode . lsp)
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
              ("C-c C-n" . go-run)
              ("C-c ."   . go-test-current-test)
              ("C-c f"   . go-test-current-file)
              ("C-c a"   . go-test-current-project))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package gotest
    :after go)

  (use-package go-tag
    :after go
    :config
    (setq go-tag-args (list "-transform" "camelcase"))))

(use-package haskell-mode
  :if (executable-find "ghc")
  :mode "\\.hs\\'"
  :config
  (setq haskell-mode-hook 'haskell-mode-defaults))

(use-package lsp-java
  :after (lsp)
  :hook (java-mode . lsp)
  :config
  (require 'dap-java)
  (shan/ide-add 'java-mode #'hydra-lsp/body))

;; Gradle
(use-package gradle-mode
  :hook (java-mode . (lambda () (gradle-mode 1)))
  :config
  (defun build-and-run()
    (interactive)
    (gradle-run "build run"))
  (define-key gradle-mode-map (kbd "C-c C-r") 'build-and-run))

(use-package mvn
  :config
  (ignore-errors
    (require 'ansi-colors)
    (defun colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (let ((inhibit-read-only t))
          (if (boundp 'compilation-filter-start)
              (ansi-color-apply-on-region compilation-filter-start (point))))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(use-package ein
  :mode
  (".*\\.ipynb\\'" . ein:ipynb-mode)
  :custom
  (ein:completion-backend 'ein:use-company-jedi-backends)
  (ein:use-auto-complete-superpack t))

(use-package lua-mode
  :after (company)
  :mode
  (("\\.lua\\'" . lua-mode))
  :hook
  (lua-mode . company-mode))

(use-package tuareg
  :if (and (executable-find "ocaml")
           (executable-find "npm")
           t)
  :ensure-system-package (ocaml-language-server . "npm install -g ocaml-language-server")
  :after (lsp)
  :hook
  (tuareg-mode . lsp)
  :mode
  (("\\.ml[ip]?\\'"                           . tuareg-mode)
   ("\\.mly\\'"                               . tuareg-menhir-mode)
   ("[./]opam_?\\'"                           . tuareg-opam-mode)
   ("\\(?:\\`\\|/\\)jbuild\\(?:\\.inc\\)?\\'" . tuareg-jbuild-mode)
   ("\\.eliomi?\\'"                           . tuareg-mode))
  :custom
  (tuareg-match-patterns-aligned t)
  (tuareg-indent-align-with-first-arg t)
  :config
  (shan/ide-add 'tuareg-mode #'hydra-lsp/body))

(use-package pip-requirements
  :mode
  ("requirements\\.txt" . pip-requirements-mode)
  :init
  (progn
    (shan/copy-hooks-to text-mode-hook 'pip-requirements-mode)))

(use-package python
  :if (executable-find "pyls")
  :after (lsp)
  :ensure nil
  :defer t
  :hook
  (python-mode . lsp)
  :custom
  (python-indent 4)
  (python-shell-interpreter shan/python-executable)
  ;; Required for MacOS, prevents newlines from being displayed as ^G
  (python-shell-interpreter-args (if (eq system-type 'darwin) "-c exec('__import__(\\'readline\\')') -i" "-i"))
  ;; (gud-pdb-command-name (concat shan/python-executable " -m pdb"))
  (python-fill-docstring-style 'pep-257)
  (py-split-window-on-execute t)
  :config
  (shan/ide-add 'python-mode #'hydra-lsp/body))

(use-package ess
  :defer t
  :mode
  ("\\.jl\\'" . ess-julia-mode)
  ("\\.[rR]\\'" . ess-r-mode))

(use-package restclient-mode
  :ensure restclient
  :mode ("\\.http\\'"))

(use-package web-mode
  :mode
  (("\\.html?\\'"       . web-mode)
   ("\\.phtml\\'"       . web-mode)
   ("\\.tpl\\.php\\'"   . web-mode)
   ("\\.blade\\.php\\'" . web-mode)
   ("\\.php$"           . my/php-setup)
   ("\\.[agj]sp\\'"     . web-mode)
   ("\\.as[cp]x\\'"     . web-mode)
   ("\\.erb\\'"         . web-mode)
   ("\\.mustache\\'"    . web-mode)
   ("\\.djhtml\\'"      . web-mode)
   ("\\.jsx\\'"         . web-mode)
   ("\\.tsx\\'"         . web-mode))
  :config
  ;; Highlight the element under the cursor.
  (setq-default web-mode-enable-current-element-highlight t)
  ;; built in color for most themes dont work well with my eyes
  (eval-after-load "web-mode"
    '(set-face-background 'web-mode-current-element-highlight-face "LightCoral"))
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))

(defun shan/emmet-mode-cheatsheet ()
  "Open emmet mode cheatsheet"
  (interactive)
  (browse-url-generic "https://docs.emmet.io/cheatsheet-a5.pdf"))

(use-package emmet-mode
  :hook
  ((css-mode  . emmet-mode)
   (php-mode  . emmet-mode)
   (sgml-mode . emmet-mode)
   (rjsx-mode . emmet-mode)
   (web-mode  . emmet-mode)))

(use-package typescript-mode
  :hook
  (typescript-mode . lsp)
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (shan/ide-add 'typescript-mode #'hydra-lsp/body))

(use-package add-node-modules-path
  :hook
  ((web-mode . add-node-modules-path)
   (rjsx-mode . add-node-modules-path)))

(use-package prettier-js
  :hook
  ((js-mode . prettier-js-mode)
   (typescript-mode . prettier-js-mode)
   (rjsx-mode . prettier-js-mode)))

(use-package tide
  :after
  (typescript-mode js2-mode company flycheck)
  :hook
  (((js2-mode . typescript-mode) . tide-setup)
   ((js2-mode . typescript-mode) . tide-hl-identifier-mode)
   (before-save . tide-format-before-save))
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint))

(use-package rjsx-mode
  :mode
  (("\\.js\\'"   . rjsx-mode)
   ("\\.jsx\\'"  . rjsx-mode)
   ("\\.json\\'" . javascript-mode))
  :magic ("/\\*\\* @jsx React\\.DOM \\*/" "^import React")
  :init
  (setq-default rjsx-basic-offset 2)
  (setq-default rjsx-global-externs '("module" "require" "assert" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON")))

(use-package react-snippets
  :after yasnippet)

(use-package vue-html-mode)

(use-package vue-mode
  :defer t
  :mode
  (("\\.vue\\'"  . vue-mode)))

(use-package artist
  :config
  ;; this is from emacswiki
  (defun shan/artist-ido-select-operation (type)
    "Use ido to select a drawing operation in artist-mode"
    (interactive (list (ido-completing-read "Drawing operation: "
                                            (list "Pen" "Pen Line" "line" "straight line" "rectangle"
                                                  "square" "poly-line" "straight poly-line" "ellipse"
                                                  "circle" "text see-thru" "text-overwrite" "spray-can"
                                                  "erase char" "erase rectangle" "vaporize line" "vaporize lines"
                                                  "cut rectangle" "cut square" "copy rectangle" "copy square"
                                                  "paste" "flood-fill"))))
    (artist-select-operation type))

  ;; also from emacswiki
  (defun shan/artist-ido-select-settings (type)
    "Use ido to select a setting to change in artist-mode"
    (interactive (list (ido-completing-read "Setting: "
                                            (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
                                                  "Rubber-banding" "Trimming" "Borders"))))
    (if (equal type "Spray-size")
        (artist-select-operation "spray set size")
      (call-interactively (artist-fc-get-fn-from-symbol
                           (cdr (assoc type '(("Set Fill" . set-fill)
                                              ("Set Line" . set-line)
                                              ("Set Erase" . set-erase)
                                              ("Rubber-banding" . rubber-band)
                                              ("Trimming" . trimming)
                                              ("Borders" . borders)
                                              ("Spray-chars" . spray-chars))))))))

  (pretty-hydra-define hydra-artist (:exit t :color pink :title " Artist" :quit-key "q")
    ("Find"
     (("a" artist-mouse-choose-operation "touch all ops")
      ("o" shan/artist-ido-select-operation "ido ops")
      ("s" shan/artist-ido-select-settings "ido settings"))
     "Drawing"
     (("l" artist-select-op-line "line")
      ("r" artist-select-op-rectangle "rectangle")
      ("p" artist-select-op-poly-line "polyline")
      ("e" artist-select-op-ellipse "ellipse"))
     "Edit"
     (("w" artist-select-op-copy-rectangle "copy")
      ("y" artist-select-op-paste "paste")
      ("c" artist-select-op-cut-rectangle "cut")
      ("f" artist-select-op-flood-fill "flood fill"))
     " Exit"
     (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))

  (shan/ide-add 'picture-mode #'hydra-artist/body))

(use-package gnuplot)

(use-package gnuplot-mode
  :mode
  ("\\.gp\\'" "\\.gnuplot\\'"))

(use-package mermaid-mode
  :if (executable-find "mmdc")
  :load-path "site-lisp/mermaid"
  :mode
  (("\\.mmd\\'" . mermaid-mode)
   ("\\.mermaid\\'" . mermaid-mode))
  :init
  (setq mermaid-mmdc-location (executable-find "mmdc")))

(use-package plantuml-mode
  :if (file-exists-p shan/plantuml-path)
  :mode
  ("\\.\\(plant\\)?uml\\'" . plantuml-mode)
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path shan/plantuml-path)
  (plantuml-java-options "")
  (plantuml-output-type "png")
  (plantuml-options "-charset UTF-8"))

(use-package csv-mode)

(use-package dhall-mode)

(use-package editorconfig
  :hook
  ((prog-mode text-mode) . editorconfig-mode)
  :config
  (editorconfig-mode 1))

(use-package groovy-mode
  :defer t
  :mode
  (("\\.groovy$" . groovy-mode)
   ("\\.gradle$" . groovy-mode)))

(use-package info
  :mode
  ("\\.info\\'" . info-mode))

(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode)
  :init
  (setq-default js-indent-level 2))

(use-package markdown-mode
  :mode
  ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

(use-package markdown-preview-mode
  :if (executable-find "pandoc")
  :after (markdown-mode)
  :custom
  (markdown-command (executable-find "pandoc"))

  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
            $(document).on('mdContentChange', function() {
              $('pre code').each(function(i, block)  {
                hljs.highlightBlock(block);
              });
            });
          </script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")

         "<style>
            .markdown-body {
              box-sizing: border-box;
              min-width: 200px;
              max-width: 980px;
              margin: 0 auto;
              padding: 45px;
            }

            @media (max-width: 767px) { .markdown-body { padding: 15px; } }
          </style>")))

(use-package pkgbuild-mode
  :mode
  (("/PKGBUILD/" . pkgbuild-mode)))

(use-package protobuf-mode)

(use-package toml-mode)

(use-package yaml-mode
  :mode
  (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
   ("\\.raml\\'" . yaml-mode)))

(use-package flycheck-yamllint
  :hook
  (flycheck-mode . flycheck-yamllint-setup))

(use-package dockerfile-mode
  :mode
  (("Dockerfile'"       . dockerfile-mode)
   ("\\.Dockerfile\\'"  . dockerfile-mode))
  :init
  (progn
    (shan/copy-hooks-to text-mode-hook 'dockerfile-mode-hook)))

;; Emacs interface to docker
(use-package docker)

(use-package kubernetes
  :commands
  (kubernetes-overview))

(use-package graphql)

(use-package graphql-mode
  :mode
  (("\\.\\(gql\\|graphql\\)\\'" . graphql-mode))
  :config
  (defun shan/set-graphql-url()
    (interactive)
    (let ((shan/user-input '("http://localhost:8000/api/graphql/query"
                             "http://localhost:3000" "Manual")))
      (ivy-read "Set graphql url: " shan/user-input
                :action #'(lambda(arg)
                            (setq graphql-url (if (string= arg "Manual")
                                                  (read-string "Enter graphql url:") arg)))
                :caller 'shan/set-graphql-url))))

(use-package sql
  :mode
  (("\\.\\(sql\\|psql\\|hql\\|mysql\\|q\\)\\'" . sql-mode))
  :hook
  (sql-mode . (lambda ()
                (sql-highlight-mysql-keywords))))

(use-package sql-indent
  :init
  (setq-default sql-indent-offset tab-width))

(use-package org
  :mode
  ("\\.\\(org\\|ORG\\)\\'" . org-mode)
  :ensure org-plus-contrib
  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-file-apps
   '((auto-mode . emacs)
     ("\\.x?html?\\'" . "/usr/bin/firefox -private-window %s")
     ("\\.pdf\\(::[0-9]+\\)?\\'" . "epdfview %s")))

  (org-directory "~/.orgfiles")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-export-html-postamble nil)

  (org-image-actual-width 480)
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)

  ;; TODO: choose better colors
  ;; (org-todo-keyword-faces
  ;;  '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
  ;;    ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
  ;;    ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
  ;;    ("DONE" :foreground "#50a14f" :weight normal :underline t)
  ;;    ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)))

  ;; (org-babel-python-command "ipython3 -i --simple-prompt")

  (org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  :config
  ;; (use-package ob-ipython)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa      . t)
     (dot        . t)
     (emacs-lisp . t)
     (gnuplot    . t)
     (js         . t)
     (latex      . t)
     (ocaml      . t)
     (org        . t)
     (plantuml   . t)
     ;; (python     . t)
     (shell      . t)
     (R          . t)
     ))

  (add-to-list 'org-src-lang-modes
               '("plantuml" . fundamental)))

(use-package toc-org
  :after (org)
  :hook
  (org-mode . toc-org-enable))

(use-package org-bullets
  :after (org)
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("⁖"))
  :config
  ;; trying to figure out how to change the size without affecting anything else
  ;; (setq org-bullets-face-name 'org-bullet-face)
  ;; (set-face-attribute 'org-bullet-face nil
  ;;                     :height 1.50
  ;;                     :weight 'bold)

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

(use-package org-fancy-priorities
  :after (org)
  :diminish
  :defines org-fancy-priority-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  ;; colors of priorities
  (setq org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)
          (?D . (:foreground "#87ceeb"))))

  ;; symbol for priorities
  (setq org-fancy-priorities-list '("⬛" "⬛" "⬛" "⬛")))

(use-package px)

(use-package ox-gfm
  :after (org))

(use-package ox-pandoc)

(use-package ox-reveal
  :custom
  (org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/") ;; possibly make this local
  (org-reveal-mathjax t))

(use-package htmlize)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

(shan/add-list-to-list 'org-structure-template-alist '(("el" . "src emacs-lisp\n")
                                                       ("ts" . "src ts\n")
                                                       ("js" . "src js\n")
                                                       ("py" . "src python\n")
                                                       ("r" . "src R\n")
                                                       ("sh" . "src shell\n")))

(use-package olivetti
  :diminish
  :bind
  ("<f7>" . olivetti-mode)
  :init
  (setq olivetti-body-width 0.618))

(use-package tex
  :ensure auctex
  :mode
  ("\\.tex\\'" . LaTeX-mode)
  :config
  (pretty-hydra-define hydra-latex (:exit t :color pink :title " Latex" :quit-key "q")
    (
     " Exit"
     (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))
  ;; interestingly enough, auto ide doesnt like the latex formatted latex
  (shan/ide-add 'latex-mode #'hydra-latex/body))

(use-package auctex-latexmk
  :hook
  (LaTeX-mode . flymake-mode)
  :init
  (setq TeX-show-compilation nil)
  (setq TeX-save-query nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  ;; (setq TeX-PDF-mode t)
  (auctex-latexmk-setup)
  :config
  ;; use flymake as checker on latex docs
  (defun flymake-get-tex-args (file-name)
    (list "pdflatex"
          (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex))

(use-package company-auctex
  :after (auctex company)
  :config
  (company-auctex-init))

(use-package company-math
  :after (auctex company)
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package reftex
  :after auctex
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t))

(use-package pdf-view
  :ensure pdf-tools
  :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :functions my-pdf-view-set-midnight-colors
  :commands pdf-view-midnight-minor-mode
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (after-load-theme . my-pdf-view-set-dark-theme)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init
  (setq pdf-annot-activate-created-annotations t)

  (defun my-pdf-view-set-midnight-colors ()
    "Set pdf-view midnight colors."
    (setq pdf-view-midnight-colors
          `(,(face-foreground 'default) . ,(face-background 'default))))

  (defun my-pdf-view-set-dark-theme ()
    "Set pdf-view midnight theme as color theme."
    (my-pdf-view-set-midnight-colors)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pdf-view-mode)
          (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))
  :config
  ;; WORKAROUND: Fix compilation errors on macOS.
  ;; @see https://github.com/politza/pdf-tools/issues/480
  (when shan/is-mac?
    (setenv "PKG_CONFIG_PATH"
            "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
  ;; (pdf-tools-install t nil t t) ;; FIRST TIME INSTALL USAGE
  (pdf-tools-install)

  (my-pdf-view-set-midnight-colors)

  ;; FIXME: Support retina
  ;; @see https://emacs-china.org/t/pdf-tools-mac-retina-display/10243/
  ;; and https://github.com/politza/pdf-tools/pull/501/
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (with-no-warnings
    (defun pdf-view-use-scaling-p ()
      "Return t if scaling should be used."
      (and (or (and (eq system-type 'darwin) (string-equal emacs-version "27.0.50"))
               (memq (pdf-view-image-type)
                     '(imagemagick image-io)))
           pdf-view-use-scaling))
    (defun pdf-view-create-page (page &optional window)
      "Create an image of PAGE for display on WINDOW."
      (let* ((size (pdf-view-desired-image-size page window))
             (width (if (not (pdf-view-use-scaling-p))
                        (car size)
                      (* 2 (car size))))
             (data (pdf-cache-renderpage
                    page width width))
             (hotspots (pdf-view-apply-hotspot-functions
                        window page size)))
        (pdf-view-create-image data
          :width width
          :scale (if (pdf-view-use-scaling-p) 0.5 1)
          :map hotspots
          :pointer 'arrow))))

  ;; Recover last viewed position
  (when (>= emacs-major-version 26)
    (use-package pdf-view-restore
      :hook (pdf-view-mode . pdf-view-restore-mode)
      :init (setq pdf-view-restore-filename
                  (locate-user-emacs-file ".pdf-view-restore")))))

(use-package nov
  :after (olivetti)
  :mode
  ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . shan/my-nov-setup)
  :bind
  (:map nov-mode-map
        ("C-p" . nov-previous-document)
        ("C-n" . nov-next-document)
        ("p"   . nov-scroll-up)
        ("n"   . nov-scroll-down))
  :config
  (defun shan/my-nov-setup ()
    (if (fboundp 'olivetti-mode)
        (olivetti-mode 1)))

  (setq nov-variable-pitch nil)
  (setq nov-text-width 72))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode 1))

(use-package flyspell
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package flyspell-popup
  :after (flyspell)
  :preface
  ;; move point to previous error
  ;; based on code by hatschipuh at
  ;; http://emacs.stackexchange.com/a/14912/2017
  (defun flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn (if (= flyspell-old-pos-error min)
                       ;; goto beginning of buffer
                       (progn (message "Restarting from end of buffer")
                              (goto-char (point-max)))
                     (backward-word 1))
                   (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r)
                                  (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; save the current location for next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn (message "No more miss-spelled word!")
                   (setq arg 0))
          (forward-word)))))
  (defun shan/flyspell-next-word()
    "Jump to next miss-pelled word and pop-up correction"
    (interactive)
    (flyspell-goto-next-error)
    (flyspell-popup-correct))
  (defun shan/flyspell-prev-word()
    "Jump to prev miss-pelled word and pop-up correction"
    (interactive)
    (flyspell-goto-previous-error (char-after 1))
    (flyspell-popup-correct))
  :bind
  (:map flyspell-mode-map
        ("C-,"   . shan/flyspell-next-word)
        ("C-M-," . shan/flyspell-prev-word)))

(use-package expand-region
  :defer t
  :bind
  ("C-=" . er/expand-region))

(setq-default require-final-newline t)
(global-subword-mode t)
(delete-selection-mode t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(global-set-key [remap fill-paragraph] #'shan/fill-or-unfill)

(use-package aggressive-indent
  :defer t)

(use-package avy
  :bind
  ("C-'" . avy-goto-char-2)
  :custom
  (avy-keys shan/home-row))

(use-package ace-window
  :bind
  ("C-x C-w" . ace-window)
  :custom
  (aw-keys shan/home-row))

(use-package pubmed
  :commands (pubmed-search pubmed-advanced-search))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-args '("-private")
      browse-url-firefox-program "firefox"
      browse-url-generic-program "firefox")

(defun shan/elfeed-sync-database ()
  "Wrapper to load the elfeed db from disk and update it"
  (interactive)
  (elfeed-db-load)
  (elfeed-update))

(defun shan/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (shan/elfeed-sync-database)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun elfeed-mark-all-as-read ()
  "Wrapper to mark all elfeed entries in a buffer as read"
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(use-package elfeed
  :if (file-exists-p shan/elfeed-file)
  :bind
  (:map elfeed-search-mode-map
        ("q" . shan/elfeed-save-db-and-bury)
        ("Q" . shan/elfeed-save-db-and-bury)
        ("m" . elfeed-toggle-star)
        ("M" . elfeed-toggle-star))
  :custom
  (elfeed-db-directory shan/elfeed-db)
  :config
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star)))

(use-package elfeed-org
  :after (elfeed)
  :custom
  (rmh-elfeed-org-files (list shan/elfeed-file))
  :config
  (elfeed-org))

(use-package elfeed-goodies
  :after (elfeed elfeed-org)
  :config
  (elfeed-goodies/setup))

(use-package emojify
  :init
  (setq emojify-user-emojis '(("🧚" . (("name" . "Fairy")
                                       ("image" . "~/.emacs.d/emoji/fairy.png")
                                       ("style" . "unicode")))))
  (setq emojify-point-entered-behaviour 'uncover)
  (setq emojify-show-help nil)
  (global-emojify-mode)
  (emojify-set-emoji-data))

(use-package keyfreq
  :config
  (keyfreq-mode t)
  ;;(keyfreq-autosave-mode 1)
  )

(use-package wakatime-mode
  :if (and (executable-find "wakatime") (boundp 'wakatime-api-key))
  :custom
  (wakatime-cli-path (executable-find "wakatime"))
  :init
  (global-wakatime-mode))

(use-package speed-type)
(use-package origami)
(use-package demangle-mode)
(use-package academic-phrases)
(use-package powerthesaurus)
(use-package crontab-mode)
(use-package salt-mode)
(use-package sicp)
(use-package rmsbolt)                   ; A compiler output viewer
