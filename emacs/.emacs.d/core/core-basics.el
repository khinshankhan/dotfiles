;;; core-basics.el --- Core module that sets up the basic configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Contains basic configuration.
;;; Code:

(require 'core-straight)

;; Encoding
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(setq-default locale-coding-system 'utf-8)
(dolist (fn '(set-terminal-coding-system set-keyboard-coding-system set-selection-coding-system prefer-coding-system))
  (if (fboundp fn)
      (funcall fn 'utf-8)))

(package! unidecode)

;; Backups
(setq-default backup-inhibited t
              auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

;; kill processes when leaving emacs
(when (>= emacs-major-version 26)
  (setq-default confirm-kill-processes nil))

;; You need to experience keyboard to realize keyboard master race. (`fn + f10' if need be for options though)
(setq inhibit-startup-message t)
(dolist (fn '(tool-bar-mode scroll-bar-mode menu-bar-mode))
  (if (fboundp fn)
      (funcall fn -1)))

;; Source code pro is good just the way it is. Noto just seems to break emacs(?). Symbola for emoji!
(when (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :weight 'normal
                      :width 'normal))

(add-to-list 'face-ignored-fonts "Noto Color Emoji")

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Interface
(setq-default visible-bell nil
              audible-bell nil
              ring-bell-function 'ignore)

;; TODO: look into other options which still require full words to be typed
(defalias 'yes-or-no-p (lambda (&rest _) t))
(setq-default confirm-kill-emacs nil)
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(setq-default transient-mark-mode t
              visual-line-mode t
              indent-tabs-mode nil
              tab-width 4)

;; highlights the line containing mark
(if (fboundp 'global-hl-line-mode)
    (global-hl-line-mode t))

(setq-default initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message nil)

(setq-default require-final-newline t
              vc-follow-symlinks t
              fill-column 120)

(global-subword-mode t)
(delete-selection-mode t)
(global-font-lock-mode t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(package! expand-region
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

(global-set-key [remap fill-paragraph]
                #'shan/fill-or-unfill)

(global-set-key (kbd "M-;")
                'comment-line)

(provide 'core-basics)
;;; core-basics.el ends here
