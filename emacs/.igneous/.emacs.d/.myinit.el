* Encoding

#+BEGIN_SRC emacs-lisp
(setq-default locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
#+END_SRC

* Backups

#+BEGIN_SRC emacs-lisp
(setq-default backup-inhibited t
              auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)
#+END_SRC

* Packages

#+BEGIN_SRC emacs-lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq-default use-package-always-ensure t)
#+END_SRC

* Custom Variables

#+BEGIN_SRC emacs-lisp
(defconst custom-file "/dev/zero")
#+END_SRC

* System
** MacOS

#+BEGIN_SRC emacs-lisp
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))
#+END_SRC

* GUI

#+BEGIN_SRC emacs-lisp
(when (window-system)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))
#+END_SRC

* Appearance
** Font

#+BEGIN_SRC emacs-lisp
(set-frame-font (if (memq window-system '(mac ns)) "Menlo-14" "FiraCode-12") nil t)
#+END_SRC

** Theme

#+BEGIN_SRC emacs-lisp
(use-package doom-themes
  :config
  (doom-themes-org-config)
  (load-theme 'doom-vibrant t))
#+END_SRC

** Modeline

#+BEGIN_SRC emacs-lisp
(line-number-mode t)
(column-number-mode t)

(use-package doom-modeline
  :custom
  (doom-modeline-python-executable "python3.7")
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-version t)
  :config
  (doom-modeline-mode))
#+END_SRC

* Functions
** Dependencies

#+BEGIN_SRC emacs-lisp
(use-package popwin)
#+END_SRC

** Definitions

#+BEGIN_SRC emacs-lisp
(defun ign/split-window-right ()
  (interactive)
  (split-window-right)
  (balance-windows))

(defun ign/kill-and-balance-window ()
  (interactive)
  (if (= (length (get-buffer-window-list (current-buffer))) 1)
      (kill-buffer-and-window)
    (delete-window))
  (balance-windows))

(defun ign/popwin-ansi-term (name)
  (popwin:display-buffer-1
   (or (get-buffer name)
       (save-window-excursion
         (ansi-term "zsh" name)))
   :default-config-keywords '(:height 15 :position :bottom :noselect nil :stick t)))

(defun ign/term-toggle ()
  (interactive)
  (if (get-buffer "*term*")
      (progn (switch-to-buffer "*term*")
	     (kill-buffer-and-window))
    (ign/popwin-ansi-term "term")))
#+END_SRC

* Interface
** Splash Screen

#+BEGIN_SRC emacs-lisp
(use-package dashboard
  :custom
  (dashboard-banner-logo-title
   (format "[Emacs ready in %.2f seconds with %d garbage collections.]"
	   (float-time (time-subtract after-init-time before-init-time)) gcs-done))
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))
#+END_SRC

** Scratch buffer

#+BEGIN_SRC emacs-lisp
(setq-default initial-major-mode 'python-mode)
#+END_SRC

** Lines

#+BEGIN_SRC emacs-lisp
(setq-default transient-mark-mode t
              visual-line-mode t)

(global-hl-line-mode 1)

(use-package linum
  :hook (prog-mode . linum-mode)
  :custom
  (linum-format " %d ")
  :config
  (set-face-underline 'linum nil))
#+END_SRC

** Scrolling

#+BEGIN_SRC emacs-lisp
(setq-default scroll-margin 0
              scroll-conservatively 10000
              scroll-preserve-screen-position t
              mouse-wheel-progressive-speed nil)
#+END_SRC

** Confirmation messages

#+BEGIN_SRC emacs-lisp
(defalias 'yes-or-no-p (lambda (&rest _) t))
(setq-default confirm-kill-emacs nil)
#+END_SRC

** Bells

#+BEGIN_SRC emacs-lisp
(setq-default visible-bell nil
              audible-bell nil
              ring-bell-function 'ignore)
#+END_SRC

* Completion Frontend

#+BEGIN_SRC emacs-lisp
(use-package ivy
  :demand
  :bind
  (:map ivy-minibuffer-map
	("RET" . ivy-alt-done))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  ("C-x b" . counsel-ibuffer))

(use-package swiper
  :bind
  ("C-s" . swiper))
#+END_SRC

* Org

#+BEGIN_SRC emacs-lisp
(use-package org
  :ensure nil
  :custom
  (org-src-fontify-natively t)
  (org-babel-python-command "python3")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (ocaml . t))))
#+END_SRC

* Programming

#+BEGIN_SRC emacs-lisp
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(show-paren-mode t)
#+END_SRC

** Git

#+BEGIN_SRC emacs-lisp
(use-package magit
  :bind
  ("C-c g" . magit-status))

(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))
#+END_SRC

** Company

#+BEGIN_SRC emacs-lisp
(use-package company
  :bind
  ("C-SPC" . company-complete)
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous))
  :custom-face
  (company-tooltip ((t (:foreground "#ABB2BF" :background "#30343C"))))
  (company-tooltip-annotation ((t (:foreground "#ABB2BF" :background "#30343C"))))
  (company-tooltip-selection ((t (:foreground "#ABB2BF" :background "#393F49"))))
  (company-tooltip-mouse ((t (:background "#30343C"))))
  (company-tooltip-common ((t (:foreground "#ABB2BF" :background "#30343C"))))
  (company-tooltip-common-selection ((t (:foreground "#ABB2BF" :background "#393F49"))))
  (company-preview ((t (:background "#30343C"))))
  (company-preview-common ((t (:foreground "#ABB2BF" :background "#30343C"))))
  (company-scrollbar-fg ((t (:background "#30343C"))))
  (company-scrollbar-bg ((t (:background "#30343C"))))
  (company-template-field ((t (:foreground "#282C34" :background "#C678DD"))))
  :custom
  (company-idle-delay 120)
  :config
  (global-company-mode t))
#+END_SRC

** Flycheck

#+BEGIN_SRC emacs-lisp
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
#+END_SRC

** Python

#+BEGIN_SRC emacs-lisp
(use-package pip-requirements)

(use-package python
  :after flycheck
  :ensure nil
  :interpreter ("ipython3" . python-mode)
  :custom
  (python-indent 4)
  (python-shell-interpreter-args "--simple-prompt -i")
  (python-fill-docstring-style 'pep-257)
  (py-split-window-on-execute t)
  (flycheck-python-pylint-executable "python3")
  (flycheck-python-pycompile-executable "python3"))

(use-package company-jedi
  :after company
  :config
  (add-to-list 'company-backends 'company-jedi))
#+END_SRC

** OCaml

#+BEGIN_SRC emacs-lisp
(use-package tuareg
  :mode ("\\.ml[ly]\\'" . tuareg-menhir-mode)
  :custom
  (tuareg-match-patterns-aligned t)
  (tuareg-indent-align-with-first-arg t))

(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :config
  (when (file-exists-p "~/.emacs.d/opam-user-setup.el")
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")))
#+END_SRC

** C

#+BEGIN_SRC emacs-lisp
(use-package cc-mode
  :ensure nil
  :hook
  (c-mode . (lambda () (setq indent-tabs-mode t)
	      (global-aggressive-indent-mode -1)))
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))
#+END_SRC

** R

#+BEGIN_SRC emacs-lisp
(use-package ess
  :pin melpa-stable
  :mode
  ("\\.[rR]\\'" . R-mode)
  :config
  (require 'ess-site))
#+END_SRC

* Text Editing

#+BEGIN_SRC emacs-lisp
(setq-default require-final-newline t)
(global-subword-mode 1)
(delete-selection-mode t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode 1))
#+END_SRC

* Text Navigation

#+BEGIN_SRC emacs-lisp
(use-package avy
  :bind
  ("C-'" . avy-goto-char-2)
  :custom
  (avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package ace-window
  :bind
  ("C-x C-w" . ace-window)
  :custom
  (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))
#+END_SRC

* Bindings

#+BEGIN_SRC emacs-lisp
(define-key key-translation-map (kbd "C-t") (kbd "C-x"))
(define-key key-translation-map (kbd "M-t") (kbd "M-x"))

(use-package bind-key)
(bind-key* "C-x C-k" 'ign/kill-and-balance-window)
(bind-key* "C-c w" 'ign/split-window-right)
(bind-key* "C-c t" 'ign/term-toggle)
(bind-key* "M-/" 'hippie-expand)
#+END_SRC

** Which-key

#+BEGIN_SRC emacs-lisp
(use-package which-key
  :demand
  :config
  (which-key-mode)
  :bind
  ("C-h m" . which-key-show-major-mode)
  ("C-h b" . which-key-show-top-level))
#+END_SRC

* Community
** Browser

#+BEGIN_SRC emacs-lisp
(setq-default browse-url-browser-function 'browse-url-chromium)
#+END_SRC

** Discord

#+BEGIN_SRC emacs-lisp
(use-package elcord
  :config
  (elcord-mode))
#+END_SRC
